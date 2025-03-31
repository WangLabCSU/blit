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
    verbose = TRUE, echo_command = verbose,
    call = caller_call()) {
    assert_bool(verbose, call = call)

    # prepare the shell -------------------------
    if (.Platform$OS.type == "windows") {
        # https://stackoverflow.com/questions/605686/how-to-write-a-multiline-command
        # for cmd "^"
        # for powershell "`"
        shell <- shell %||% "cmd"
        arg <- switch(shell,
            cmd = "/C",
            powershell = c("-ExecutionPolicy", "Bypass", "-File")
        )
        fileext <- switch(shell,
            cmd = "bat",
            powershell = "ps1"
        )
        shell <- paste(shell, "exe", sep = ".")
    } else {
        shell <- shell %||% "sh" # or "bash"
        arg <- NULL
        fileext <- "sh"
    }
    shell0 <- Sys.which(shell)
    if (!nzchar(shell0)) {
        cli::cli_abort("Cannot find shell {.field {shell}}", call = call)
    }
    shell <- shell0

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
        if (verbose) cli::cli_inform("Working Directory: {.path {wd}}")
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
        old <- Sys.getenv(
            names(envvar),
            names = TRUE,
            unset = NA_character_
        )
        on.exit(set_envvar(old), add = TRUE)
        set_envvar(envvar_parse(envvar))
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
    if (is_processx_std_file(stdin, pipe = FALSE)) {
        content[1L] <- paste(content[1L], "<", shQuote(stdin))
        stdin <- TRUE
    }
    if (length(content) > 1L) {
        content[-length(content)] <- sprintf("%s |", content[-length(content)])
        content[-1L] <- paste0("    ", content[-1L])
    }

    # write the content to the script
    script <- tempfile(pkg_nm(), fileext = sprintf(".%s", fileext))
    script <- normalizePath(script, winslash = "/", mustWork = FALSE)
    write_lines(content, script)

    # ensure the file is executable
    file_executable(script)

    # on_start ------------------
    on_start_list <- lapply(command_series, function(cmd) cmd$get_on_start())
    on_start_list <- unlist(on_start_list, FALSE, FALSE)
    on_start_list <- c(on_start_list, .subset2(command, "on_start"))

    # on_exit -------------------
    on_exit_list <- lapply(command_series, function(cmd) cmd$get_on_exit())
    on_exit_list <- unlist(on_exit_list, FALSE, FALSE)
    on_exit_list <- c(on_exit_list, .subset2(command, "on_exit"))

    # remove the script created -------------------------
    on_exit_list <- c(on_exit_list, list(as.call(list(
        function() {
            if (file.exists(script)) {
                # if (verbose) {
                #     cli::cli_inform(
                #         "Removing temporaty script {.path {script}}"
                #     )
                # }
                file.remove(script)
            }
        }
    ))))

    # on_fail ------------------
    on_fail_list <- lapply(command_series, function(cmd) cmd$get_on_fail())
    on_fail_list <- unlist(on_fail_list, FALSE, FALSE)
    on_fail_list <- c(on_fail_list, .subset2(command, "on_fail"))

    # on_succeed ------------------
    on_succeed_list <- lapply(command_series, function(cmd) {
        cmd$get_on_succeed()
    })
    on_succeed_list <- unlist(on_succeed_list, FALSE, FALSE)
    on_succeed_list <- c(on_succeed_list, .subset2(command, "on_succeed"))

    if (echo_command) {
        cli::cli_text(paste(
            "Running command ({as.character(Sys.time(), digits = 0)}):",
            "{.field {paste(content, collapse = ' ')}}"
        ))
        cli::cat_line()
    }

    # execute the command ----------------------
    BlitProcess$new(
        command = shell,
        args = c(arg, script),
        wd = wd,
        env = NULL,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        cleanup_tree = TRUE,
        .blit_verbose = verbose,
        .blit_stdout_callback = stdout_callback,
        .blit_stderr_callback = stderr_callback,
        .blit_start = blit_schedule(on_start_list),
        .blit_exit = blit_schedule(on_exit_list),
        .blit_fail = blit_schedule(on_fail_list),
        .blit_succeed = blit_schedule(on_succeed_list)
    )
}

blit_schedule <- function(expressions) {
    if (length(expressions) == 0L) return(NULL) # styler: off
    function() {
        for (expression in expressions) {
            rlang::try_fetch(
                rlang::eval_tidy(expression),
                error = function(cnd) cli::cli_warn(conditionMessage(cnd))
            )
        }
    }
}

#' @keywords internal
BlitProcess <- R6Class(
    "BlitProcess",
    inherit = processx::process,
    public = list(
        # fmt: skip
        initialize = function(...,
                              stdin = NULL,
                              stdout = NULL,
                              stderr = NULL,
                              env = NULL,
                              .blit_stdout_callback = NULL,
                              .blit_stderr_callback = NULL,
                              .blit_start = NULL,
                              .blit_exit = NULL,
                              .blit_fail = NULL,
                              .blit_succeed = NULL,
                              .blit_verbose = FALSE) {
            # prepare the stdout and stderr ---------------
            private$.blit_stdout <- stdout
            private$.blit_stdout_callback <- .blit_stdout_callback
            private$.blit_stderr <- stderr
            private$.blit_stderr_callback <- .blit_stderr_callback

            # translate the stdin, stdout and stderr from `blit` into `processx`
            if (isFALSE(stdin)) {
                stdin <- NULL
            } else if (isTRUE(stdin)) {
                if (processx::is_valid_fd(0L)) {
                    stdin <- ""
                } else {
                    stdin <- NULL
                }
            }

            if (is.null(stdout)) {

            } else if (isFALSE(stdout)) {
                stdout <- NULL
            } else if (inherits(stdout, "connection")) {
                # we need echo the stdout
                stdout <- "|"
            } else if (!is.null(.blit_stdout_callback)) {
                # when callback is not `NULL`, we must use pipe
                if (isTRUE(stdout) || is_processx_std_file(stdout)) {
                    stdout <- "|"
                }
            } else if (isTRUE(stdout)) {
                # when the callback is `NULL`, we try to inherit the R
                # process stdout
                if (processx::is_valid_fd(1L)) {
                    stdout <- ""
                } else {
                    stdout <- "|"
                }
            }

            if (is.null(stderr)) {

            } else if (isFALSE(stderr)) {
                stderr <- NULL
            } else if (inherits(stderr, "connection")) {
                # we need echo the stderr
                stderr <- "|"
            } else if (!is.null(.blit_stderr_callback)) {
                # when callback is not `NULL`, we must use pipe
                if (isTRUE(stderr) ||
                    is_processx_std_file(stderr, redirect = TRUE)) {
                    stderr <- "|"
                }
            } else if (isTRUE(stderr)) {
                # when the callback is `NULL`, we try to inherit the R
                # process stderr
                if (processx::is_valid_fd(2L)) {
                    stderr <- ""
                } else {
                    stderr <- "|"
                }
            }
            if (!is.null(env) && !inherits(env, "AsIs")) {
                env <- c(env, "current")
            }

            super$initialize(
                ...,
                env = env,
                stdin = stdin,
                stdout = stdout,
                stderr = stderr
            )

            # workaround for https://github.com/r-lib/processx/issues/394
            private$starttime <- Sys.time()

            if (is.function(.blit_start)) .blit_start()
            if (is.function(.blit_exit)) private$.blit_exit <- .blit_exit
            if (is.function(.blit_succeed)) {
                private$.blit_succeed <- .blit_succeed
            }
            if (is.function(.blit_fail)) {
                private$.blit_fail <- .blit_fail
            }
            private$.blit_verbose <- .blit_verbose
            # always ensure the connection methods get prepared
            private$.blit_stdout_prepare()
            private$.blit_stderr_prepare()
        },
        # @param poll_timeout Timeout in milliseconds, for the wait or the I/O
        # polling.
        # @return A boolean value indicates whether the process is active
        .blit_collect_active = function(timeout = NULL, poll_timeout = 200) {
            if (out <- self$is_alive()) {
                # Timeout? Maybe finished by now...
                # fmt: skip
                start_time <- private$starttime
                if (!is.null(timeout) &&
                    is.finite(timeout) &&
                    (diff <- Sys.time() - start_time) > timeout) {
                    self$.blit_kill(close_connections = FALSE)
                    private$.blit_timeout <- diff
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
        .blit_run = function(timeout = NULL, spinner = TRUE) {
            # We make sure that all stdout and stderr have been collected,
            # the process is eliminated and the connections are closed
            on.exit(self$.blit_finish(), add = TRUE)
            do_run <- function() {
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
                    while (self$.blit_collect_active(timeout, 200)) {
                        cli::cli_progress_update(
                            0L,
                            id = progress, force = TRUE
                        )
                    }
                } else {
                    while (self$.blit_collect_active(timeout)) {
                    }
                }
                # Needed to get the exit status
                self$wait()
            }
            rlang::try_fetch(
                do_run(),
                interrupt = function(cnd) {
                    # always ensure we only kill the process, but don't close
                    # the connections, so that `$.blit_finish()` method
                    # will collect all the stdout and stderr
                    self$.blit_kill(close_connections = FALSE)
                    cli::cli_warn(
                        "System command interrupted",
                        class = "system_command_interrupt"
                    )
                    invokeRestart("abort")
                }
            )
        },
        .blit_kill = function(close_connections = TRUE) {
            self$kill(close_connections = close_connections)
            if (private$cleanup_tree) {
                self$kill_tree(close_connections = close_connections)
            }
            invisible(self)
        },
        # Must run `$wait()` method to get the exit status before using
        .blit_finish = function() {
            # `$get_exit_status` returns the exit code of the process if it
            # has finished and `NULL` otherwise. On Unix, in some rare
            # cases, the exit status might be `NA`. This happens if another
            # package (or R itself) overwrites the processx `SIGCHLD`
            # handler, after the processx process has started. In these
            # cases processx cannot determine the real exit status of the
            # process. One such package is parallel, if used with fork
            # clusters, e.g. through the `parallel::mcparallel()` function.
            # # complete the collection of stdout
            if (!is.null(status <- self$get_exit_status())) {
                if (private$.blit_finished) {
                    return(invisible(self))
                }

                # complete the collection of stdout -----------------
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
                    }
                }

                # complete the collection of stderr -----------------
                if (self$has_error_connection()) {
                    while (self$is_incomplete_error()) {
                        self$poll_io(-1)
                        if (!private$.blit_collect_stderr()) break
                    }
                    # ensure the `stderrr_remain` added
                    if (!is.null(line <- private$.blit_stderr_remain) &&
                        nzchar(line)) {
                        if (!is.null(private$.blit_stderr_callback)) {
                            line <- private$.blit_stderr_callback(line, self)
                        }
                        private$.blit_stderr_push(line)
                    }
                    if (!is.null(private$.blit_stderr_done)) {
                        private$.blit_stderr_done()
                    }
                }

                # run the cleanup function --------------------------
                if (is.na(status) || status != 0L) {
                    if (!is.null(private$.blit_fail)) private$.blit_fail()
                } else {
                    if (!is.null(private$.blit_succeed)) private$.blit_succeed()
                }
                if (!is.null(private$.blit_exit)) private$.blit_exit()
                self$.blit_kill(close_connections = TRUE)
                if (private$.blit_verbose) {
                    cli::cli_inform("Command process finished")
                }
                private$.blit_finished <- TRUE
            } else {
                cli::cli_abort("Please using `$wait()` method before finishing")
            }
            invisible(self)
        },
        .blit_signal = function(prefix = NULL) {
            status <- self$get_exit_status()
            if (!is.null(private$.blit_timeout)) {
                msg <- sprintf(
                    "System command timed out in %s (status: %d)",
                    format(private$.blit_timeout, digits = 2), status
                )
                if (!is.null(prefix)) msg <- paste0(prefix, msg)
                cli::cli_warn(msg, class = "system_command_timeout")
            } else if (is.null(status)) {
                # Only occur when user manually call this method
                msg <- "System command is running"
                if (!is.null(prefix)) msg <- paste0(prefix, msg)
                cli::cli_inform(msg)
            } else if (is.na(status) || status != 0L) {
                msg <- sprintf("System command failed (status: %d)", status)
                if (!is.null(prefix)) msg <- paste0(prefix, msg)
                cli::cli_warn(msg, class = "system_command_failed")
            } else if (private$.blit_verbose) {
                msg <- "System command succeed"
                if (!is.null(prefix)) msg <- paste0(prefix, msg)
                cli::cli_inform(msg)
            }
            status
        }
    ),
    private = list(
        # A single boolean value indicates whether the process has been finished
        .blit_finished = FALSE,
        # A single time difference value
        .blit_timeout = NULL,
        # A single boolean value indicates whether the process should be verbose
        .blit_verbose = NULL,
        # A function used to cleanup
        .blit_exit = NULL,
        .blit_fail = NULL,
        .blit_succeed = NULL,
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
            if (!self$has_output_connection()) {
                return(NULL)
            }
            if (isTRUE(stdout) || is_processx_pipe(stdout)) {
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
            if (!self$has_error_connection()) {
                return(NULL)
            }
            if (isTRUE(stderr) || is_processx_pipe(stderr)) {
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
            self$.blit_finish()
            if (!is.null(super$finalize)) super$finalize()
        }
    )
)

is_processx_inherit <- function(x) rlang::is_string(x) && x == ""
is_processx_pipe <- function(x) rlang::is_string(x) && x == "|"
is_processx_string <- function(x, pipe = TRUE, redirect = FALSE) {
    rlang::is_string(x) &&
        any(x == c("", if (pipe) "|", if (redirect) "2>&1"))
}
is_processx_std_file <- function(x, pipe = TRUE, redirect = FALSE) {
    rlang::is_string(x) &&
        !(any(x == c("", if (pipe) "|", if (redirect) "2>&1")))
}
