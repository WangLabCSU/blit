# Transform the `command` object to a `processx::process` object
#' @param shell For windows, a string of "cmd" or "powershell", for others, a
#' string of "sh" or "bash".
#' @importFrom rlang caller_call
#' @keywords internal
#' @noRd
processx_command <- function(
    command,
    help,
    shell = NULL,
    stdout = TRUE,
    stderr = TRUE,
    stdin = NULL,
    stdout_callback = NULL,
    stderr_callback = NULL,
    verbose = TRUE,
    call = caller_call()
) {
    assert_bool(verbose, call = call)

    # set working directory ---------------------
    if (!is.null(wd <- .subset2(command, "wd"))) {
        # fmt: skip
        if (!dir.exists(wd) &&
                !dir.create(wd, showWarnings = FALSE)) {
            cli::cli_abort(
                "Cannot create working directory {.path {wd}}",
                call = call
            )
        }
        if (verbose) cli::cli_inform("Working Directory: {.path {wd}")
    }
    command_series <- .subset2(command, "command_series")

    # for help document, we only display the last one
    if (help) {
        command_series <- utils::tail(command_series, 1L)
    }

    # setting environment variables -------------
    if (length(envvar <- .subset2(command, "envvar"))) {
        if (verbose) {
            cli::cli_inform(
                "Setting environment variables: {.field {names(envvar)}}"
            )
        }
        old <- as.list(Sys.getenv(
            names(envvar),
            names = TRUE,
            unset = NA_character_
        ))
        on.exit(set_envvar(old), add = TRUE)
        set_envvar(envvar)
    }

    # build the command content script ----------
    content <- vapply(
        command_series,
        function(Command) {
            o <- Command$build_command(help = help, verbose = verbose)
            paste(o, collapse = " ")
        },
        character(1L),
        USE.NAMES = FALSE
    )
    if (!is.null(stdin)) {
        if (!is_processx_inherit(stdin)) {
            content[1L] <- paste(content[1L], "<", shQuote(stdin))
            stdin <- if (processx::is_valid_fd(0L)) "" else NULL
        }
    }
    if (length(content) > 1L) {
        content[-length(content)] <- sprintf("%s |", content[-length(content)])
        content[-1L] <- paste0("    ", content[-1L])
    }

    if (verbose) {
        cli::cli_text(paste(
            "Running command ({as.character(Sys.time(), digits = 0)}):",
            "{.field {paste(content, collapse = ' ')}}"
        ))
        cli::cat_line()
    }

    cleaning_list <- lapply(command_series, function(cmd) {
        out <- cmd$get_cleaning()
        cmd$reset_cleaning()
        out
    })
    cleaning_list <- unlist(cleaning_list, FALSE, FALSE)
    cleanup <- function() {
        for (cleaning in cleaning_list) {
            rlang::try_fetch(
                rlang::eval_tidy(cleaning),
                error = function(cnd) cli::cli_warn(conditionMessage(cnd))
            )
        }
    }

    # execute the command ----------------------
    BlitProcess$new(
        wd = wd,
        env = NULL,
        stdout = stdout,
        stderr = stderr,
        # try to inherit the terminal
        stdin = stdin,
        .blit_content = content,
        .blit_stdout_callback = stdout_callback,
        .blit_stderr_callback = stderr_callback,
        .blit_cleanup = cleanup,
        cleanup_tree = TRUE
    )
}

#' @keywords internal
BlitProcess <- R6Class(
    "BlitProcess",
    inherit = processx::process,
    public = list(
        .blit_content = NULL,
        .blit_script = NULL,
        # fmt: skip
        initialize = function(..., stdout, stderr,
                              .blit_stdout_callback,
                              .blit_stderr_callback,
                              .blit_content = NULL,
                              .blit_cleanup = NULL,
                              .blit_shell = NULL) {
            # prepare the shell -------------------------
            script <- tempfile(pkg_nm())
            if (.Platform$OS.type == "windows") {
                # https://stackoverflow.com/questions/605686/how-to-write-a-multiline-command
                # for cmd "^"
                # for powershell "`"
                cmd <- .blit_shell %||% "cmd"
                arg <- switch(
                    cmd,
                    cmd = "/C",
                    powershell = c("-ExecutionPolicy", "Bypass", "-File")
                )
                cmd <- paste(cmd, "exe", sep = ".")
            } else {
                cmd <- .blit_shell %||% "sh" # or "bash"
                arg <- NULL
            }

            # write the content to the script
            write_lines(.blit_content, script)

            # ensure the file is executable
            if (file.access(script, mode = 1L) != 0L) {
                Sys.chmod(script, "555")
            }

            self$.blit_content <- .blit_content
            self$.blit_script <- script

            # prepare the stdout and stderr ---------------
            private$.blit_stdout <- stdout
            private$.blit_stdout_callback <- .blit_stdout_callback
            private$.blit_stderr <- stderr
            private$.blit_stderr_callback <- .blit_stderr_callback

            # translate the stdout and stderr from `blit` into `processx`
            # fmt: skip
            if (is.null(stdout) ||
                is_processx_inherit(stdout)) {

            } else if (isFALSE(stdout)) {
                stdout <- NULL
            } else if (isTRUE(stdout) || inherits(stdout, "connection")) {
                # we need echo the stdout
                stdout <- "|"
            } else if (!is.null(.blit_stdout_callback)) {
                stdout <- "|"
            }

            # fmt: skip
            if (is.null(stderr) ||
                is_processx_inherit(stderr)) {

            } else if (isFALSE(stderr)) {
                stderr <- NULL
            } else if (is.null(stderr)) {
                stderr <- "2>&1"
            } else if (isTRUE(stderr) || inherits(stderr, "connection")) {
                stderr <- "|"
            } else if (!is.null(.blit_stderr_callback)) {
                stderr <- "|"
            }
            private$.blit_cleanup <- .blit_cleanup
            super$initialize(
                command = cmd,
                args = c(arg, script),
                ...,
                stdout = stdout,
                stderr = stderr
            )

            # always ensure the connection methods get prepared
            if (self$has_output_connection()) private$.blit_stdout_prepare()
            if (self$has_error_connection()) private$.blit_stderr_prepare()
        },
        .blit_run = function(timeout = NULL, spinner = FALSE) {
            out <- rlang::try_fetch(
                private$.blit_wait(timeout, spinner),
                interrupt = function(cnd) {
                    # in try_fetch, contition function are run before
                    # `on.exit()` in the `expr`, so we always ensure we only
                    # kill the process, but don't close the connections, so that
                    # `$.blit_complete()` method register by `on.exit()` will
                    # collect all the stdout and stderr
                    self$.blit_kill(close_connections = FALSE)
                    cli::cli_warn(
                        "System command interrupted",
                        class = "system_command_interrupt"
                    )
                    invokeRestart("abort")
                }
            )
            self$.blit_warn_timeout()
            out
        },
        .blit_warn_timeout = function(i = NULL) {
            if (!is.null(private$.blit_timeout) && private$.blit_timeout) {
                msg <- "System command timed out"
                if (!is.null(i)) msg <- sprintf("[%s] %s", i, msg)
                cli::cli_warn(msg, class = "system_command_timeout")
                private$.blit_timeout <- NULL
            }
        },

        # @param poll_timeout Timeout in milliseconds, for the wait or the I/O
        # polling.
        # fmt: skip
        .blit_active_and_collect = function(timeout = NULL,
                                            start_time = self$get_start_time(),
                                            poll_timeout = 200) {
            if (out <- self$is_alive()) {
                # Timeout? Maybe finished by now...
                # fmt: skip
                if (!is.null(timeout) &&
                    is.finite(timeout) &&
                    Sys.time() - start_time > timeout) {
                    self$.blit_kill(close_connections = FALSE)
                    private$.blit_timeout <- TRUE
                    return(FALSE)
                }
                # Otherwise just poll for 200ms, or less if a timeout is sooner.
                # We cannot poll until the end, even if there is not spinner,
                # because RStudio does not send a SIGINT to the R process,
                # so interruption does not work.
                if (!is.null(timeout) && is.finite(timeout)) {
                    remains <- timeout - (Sys.time() - start_time)
                    remains <- max(0, as.integer(as.numeric(remains) * 1000))
                    poll_timeout <- min(remains, poll_timeout)
                }
                polled <- self$poll_io(poll_timeout)

                # If output/error, then collect it
                if (any(polled == "ready")) {
                    private$.blit_collect_stdout()
                    private$.blit_collect_stderr()
                }
            }
            out
        },
        .blit_kill = function(close_connections = TRUE) {
            self$kill(close_connections = close_connections)
            if (private$cleanup_tree) {
                self$kill_tree(close_connections = close_connections)
            }
            invisible(self)
        },
        .blit_complete = function() {
            private$.blit_complete_collect()
            private$.blit_complete_cleanup()
            invisible(self)
        }
    ),
    private = list(
        # A single boolean valued indicates whether the process is timedout
        .blit_timeout = NULL,
        # A function used to cleanup
        .blit_cleanup = NULL,
        .blit_wait = function(timeout = NULL, spinner = FALSE) {
            # We make sure that all stdout and stderr have been collected
            on.exit(self$.blit_complete(), add = TRUE)

            # We make sure that the process is eliminated and the connections
            # are closed
            on.exit(self$.blit_kill(), add = TRUE)
            start_time <- self$get_start_time()
            if (spinner) {
                progress <- cli::cli_progress_bar(
                    total = 1L,
                    clear = FALSE,
                    format = paste(
                        "{cli::pb_spin} [elapsed in {cli::pb_elapsed}]",
                        "@ {as.character(Sys.time(), digits = 0)}",
                        sep = " "
                    )
                )
                # for spinner, always use 200 `poll_timeout`
                # fmt: skip
                while (self$.blit_active_and_collect(timeout, start_time, 200)) {
                    cli::cli_progress_update(
                        inc = 0L, id = progress, force = TRUE
                    )
                }
            } else {
                while (self$.blit_active_and_collect(timeout, start_time)) {
                }
            }
            super$wait()
            if (spinner) cat("\r \r")
            self$get_exit_status()
        },
        .blit_complete_cleanup = function() {
            if (file.exists(self$.blit_script)) {
                rlang::try_fetch(
                    file.remove(self$.blit_script),
                    error = function(cnd) {
                        cli::cli_warn(conditionMessage(cnd))
                        FALSE
                    }
                )
            }
            if (!is.null(private$.blit_cleanup)) {
                private$.blit_cleanup()
                private$.blit_cleanup <- NULL
            }
        },
        .blit_complete_collect = function() {
            # complete the collection of stdout
            if (self$has_output_connection()) {
                while (self$is_incomplete_output()) {
                    self$poll_io(-1)
                    if (!private$.blit_collect_stdout()) break
                }
                # ensure the stdout_remain added
                # fmt: skip
                if (!is.null(line <- private$.blit_stdout_remain) &&
                    nzchar(line)) {
                    if (!is.null(private$.blit_stdout_callback)) {
                        line <- private$.blit_stdout_callback(line, self)
                    }
                    private$.blit_stdout_push(line)
                }
                if (!is.null(private$.blit_stdout_done)) {
                    private$.blit_stdout_done()
                    private$.blit_stdout_done <- NULL
                }
            }
            private$.blit_stdout_con <- NULL
            private$.blit_stdout_push <- NULL
            private$.blit_stdout_remain <- NULL

            # complete the collection of stderr
            if (self$has_error_connection()) {
                while (self$is_incomplete_error()) {
                    self$poll_io(-1)
                    if (!private$.blit_collect_stderr()) break
                }
                # ensure the stderrr_remain added
                # fmt: skip
                if (!is.null(line <- private$.blit_stderr_remain) &&
                    nzchar(line)) {
                    if (!is.null(private$.blit_stderr_callback)) {
                        line <- private$.blit_stderr_callback(line, self)
                    }
                    private$.blit_stderr_push(line)
                }
                if (!is.null(private$.blit_stderr_done)) {
                    private$.blit_stderr_done()
                    private$.blit_stderr_done <- NULL
                }
            }
            private$.blit_stderr_con <- NULL
            private$.blit_stderr_push <- NULL
            private$.blit_stderr_remain <- NULL
        },
        .blit_collect_stdout = function(n_stdout = 2000) {
            ok <- FALSE
            if (self$has_output_connection()) {
                newout <- rlang::try_fetch(
                    {
                        o <- self$read_output(n_stdout)
                        ok <- TRUE
                        o
                    },
                    error = function(cnd) NULL
                )
                if (length(newout) && nzchar(newout)) {
                    newout <- paste0(private$.blit_stdout_remain, newout)
                    private$.blit_stdout_remain <- ""
                    lines <- .subset2(strsplit(newout, "\r?\n"), 1L)
                    nc <- nchar(newout)
                    if (substring(newout, nc, nc) != "\n") {
                        private$.blit_stdout_remain <- utils::tail(lines, 1)
                        lines <- utils::head(lines, -1)
                    }
                    if (!is.null(private$.blit_stdout_callback)) {
                        lines <- private$.blit_stdout_callback(lines, self)
                    }
                    if (is.character(lines)) private$.blit_stdout_push(lines)
                }
            }
            ok
        },
        .blit_stdout_prepare = function(stdout = private$.blit_stdout) {
            if (isTRUE(stdout)) {
                private$.blit_stdout_push <- function(text) {
                    cat(text, sep = "\n")
                }
            } else {
                if (inherits(stdout, "connection")) {
                    private$.blit_stdout_con <- stdout
                } else {
                    if (inherits(stdout, "AsIs")) {
                        private$.blit_stdout_con <- file(stdout, open = "a+b")
                    } else {
                        private$.blit_stdout_con <- file(stdout, open = "w+b")
                    }
                    private$.blit_stdout_done <- function() {
                        close(private$.blit_stdout_con)
                    }
                }
                private$.blit_stdout_push <- function(text) {
                    writeLines(text, con = private$.blit_stdout_con)
                }
            }
            private$.blit_stdout_remain <- ""
        },
        .blit_stdout_con = NULL,
        .blit_stdout_push = NULL,
        .blit_stdout_done = NULL,
        .blit_stdout_remain = NULL,
        .blit_stdout = NULL,
        .blit_stdout_callback = NULL,
        .blit_collect_stderr = function(n_stderr = 2000) {
            ok <- FALSE
            if (self$has_error_connection()) {
                newerr <- rlang::try_fetch(
                    {
                        o <- self$read_error(n_stderr)
                        ok <- TRUE
                        o
                    },
                    error = function(cnd) NULL
                )
                if (length(newerr) && nzchar(newerr)) {
                    newerr <- paste0(private$.blit_stderr_remain, newerr)
                    private$.blit_stderr_remain <- ""
                    lines <- .subset2(strsplit(newerr, "\r?\n"), 1L)
                    nc <- nchar(newerr)
                    if (substring(newerr, nc, nc) != "\n") {
                        private$.blit_stderr_remain <- utils::tail(lines, 1)
                        lines <- utils::head(lines, -1)
                    }
                    if (!is.null(private$.blit_stderr_callback)) {
                        lines <- private$.blit_stderr_callback(lines, self)
                    }
                    if (is.character(lines)) private$.blit_stderr_push(lines)
                }
            }
            ok
        },
        .blit_stderr_prepare = function(stderr = private$.blit_stderr) {
            if (isTRUE(stderr)) {
                private$.blit_stderr_push <- function(text) {
                    cat(cli::col_red(text), sep = "\n")
                }
            } else {
                if (inherits(stderr, "connection")) {
                    private$.blit_stderr_con <- stderr
                } else {
                    if (inherits(stderr, "AsIs")) {
                        private$.blit_stderr_con <- file(stderr, open = "a+b")
                    } else {
                        private$.blit_stderr_con <- file(stderr, open = "w+b")
                    }
                    private$.blit_stderr_done <- function() {
                        close(private$.blit_stderr_con)
                    }
                }
                private$.blit_stderr_push <- function(text) {
                    writeLines(text, con = private$.blit_stderr_con)
                }
            }
            private$.blit_stderr_remain <- ""
        },
        .blit_stderr_con = NULL,
        .blit_stderr_push = NULL,
        .blit_stderr_done = NULL,
        .blit_stderr_remain = NULL,
        .blit_stderr = NULL,
        .blit_stderr_callback = NULL,
        finalize = function() {
            self$.blit_complete()
            if (!is.null(super$finalize)) super$finalize()
        }
    )
)

is_processx_inherit <- function(x) rlang::is_string(x) && x == ""
is_processx_pipe <- function(x) rlang::is_string(x) && x == "|"
