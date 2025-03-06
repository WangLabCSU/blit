#' Execute a list of commands
#'
#' @param ... A list of `command` object.
#' @param stdouts,stderrs Specifies how the output streams of the child process
#' are handled. One of or a list of following values:
#'
#'  - `TRUE`: Prints the child process output to the R console. Note that the
#'    subprocess does not inherit the terminal. If the command requires a
#'    terminal, use an empty string `""` instead.
#'  - `FALSE`: Suppresses the output stream.
#'  - **string**: An empty string `""` inherits the standard output stream from
#'    the main R process (Printing in the R console). If the main R process
#'    lacks a standard output stream, such as in RGui on Windows, an error is
#'    thrown. Alternative, a file name or path to redirect the output. If a
#'    relative path is specified, it remains relative to the current working
#'    directory, even if a different directory is set using [`cmd_wd()`].
#'  - `connection`: A writable R [`connection`] object. If the connection is not
#'    [`open()`], it will be automatically opened.
#'
#' The `stderrs` parameter also accepts `NULL`, which redirects it to the same
#' connection (i.e., pipe or file) as `stdouts`.
#'
#' When a single file path is specified, the stdout/stderr of all commands will
#' be merged into this single file.
#'
#' @param stdins should the input be diverted? One of or a list of following
#' values:
#'  - `NULL`: No standard input.
#'  - **string**: An empty string `""` inherits the standard output stream from
#'    the main R process. If the main R process lacks a standard output stream,
#'    such as in RGui on Windows, an error is thrown. Alternative, a file name
#'    or path to be used as standard input.
#' @param stdout_callbacks,stderr_callbacks One of or a list of following
#' values:
#'  - `NULL`: no callback function.
#'  - `function`: A function invoked for each line of standard output or error.
#' Non-text (non-character) output will be ignored. The function should accept
#' two arguments: one for the standard output or error and another for the
#' running [`process`][processx::process] object.
#'
#' @param timeouts  Timeout in seconds. Can be a single value or a list,
#' specifying the maximum elapsed time for running the command in the separate
#' process.
#' @param threads Number of threads to use.
#' @inheritParams cmd_run
#' @return A list of exit status invisiblely.
#' @seealso
#' `r rd_seealso()`
#' @export
cmd_parallel <- function(
    ...,
    stdouts = FALSE,
    stderrs = FALSE,
    stdins = NULL,
    stdout_callbacks = NULL,
    stderr_callbacks = NULL,
    timeouts = NULL,
    threads = NULL,
    verbose = TRUE
) {
    commands <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)

    # fmt: skip
    if (!all(vapply(commands, inherits, logical(1L),
                   "command", USE.NAMES = FALSE))) {
        cli::cli_abort("{.arg ...} must be a list of {.cls command} object")
    }
    n <- length(commands)
    stdouts <- unwrap(stdouts)
    stderrs <- unwrap(stderrs)
    stdins <- unwrap(stdins)
    stdout_callbacks <- unwrap(stdout_callbacks)
    stderr_callbacks <- unwrap(stderr_callbacks)
    timeouts <- unwrap(timeouts)

    stdout_list <- check_stdio_list(stdouts, n, "...", arg = "stdouts")
    stderr_list <- check_stdio_list(
        stderrs,
        n,
        "...",
        allow_null = TRUE,
        arg = "stderrs"
    )
    stdin_list <- check_stdin_list(stdins, n, "...", arg = "stdins")
    stdout_callback_list <- check_callback_list(
        stdout_callbacks,
        n,
        "...",
        arg = "stdout_callbacks"
    )
    stderr_callback_list <- check_callback_list(
        stderr_callbacks,
        n,
        "...",
        arg = "stderr_callbacks"
    )
    timeouts <- check_timeout_list(timeouts, n, "...", arg = "timeouts")
    assert_number_whole(
        threads,
        min = 1,
        max = as.double(parallel::detectCores()),
        allow_null = TRUE
    )
    assert_bool(verbose)
    out_env <- new.env(parent = emptyenv())
    out_env$status <- out_env$process <- vector("list", n) # nolint

    do_parallel <- function() {
        poos_size <- threads %||% 1L
        progress <- cli::cli_progress_bar(
            total = n,
            clear = FALSE,
            format = paste(
                "{cli::pb_spin} {cli::pb_current}/{cli::pb_total}",
                "[{cli::pb_rate}] [elapsed in {cli::pb_elapsed}]",
                "@ {as.character(Sys.time(), digits = 0)}",
                sep = " "
            )
        )

        # pools: store the `index` of result
        # NA means this pool can be used
        pools <- rep_len(NA_integer_, poos_size)
        i <- pool <- 1L
        while (i <= n || !all(is.na(pools))) {
            pool_value <- .subset(pools, pool)
            # if i > n, we skip add task
            if (i <= n && is.na(pool_value)) {
                # this pool can be used, we add task into this pool
                # `.fn()` must return with `$collect_in_background()` method
                # For Series, cannot subset with `[[`
                out_env$process[[i]] <- processx_command(
                    .subset2(commands, i),
                    help = FALSE,
                    stdout = .subset2(stdout_list, i),
                    stderr = .subset2(stderr_list, i),
                    stdin = .subset2(stdin_list, i),
                    stdout_callback = .subset2(stdout_callback_list, i),
                    stderr_callback = .subset2(stderr_callback_list, i),
                    verbose = verbose
                )

                # We make sure that all stdout and stderr have been collected
                set_exit(out_env$process[[!!i]]$.blit_complete())

                # We make sure that the process is eliminated and the
                # connections are closed
                set_exit(out_env$process[[!!i]]$.blit_kill())

                # indicate we have occupy this pool
                pools[pool] <- i
                i <- i + 1L
            }

            # if there is a task in this pool
            # we check if we can release this pool
            if (!is.na(pool_value)) {
                proc <- .subset2(out_env$process, pool_value)
                active <- proc$.blit_active_and_collect(
                    timeout = .subset2(timeouts, pool_value),
                    poll_timeout = 200
                )
                if (!active) {
                    # collect result from this pool
                    proc$wait()

                    # it's not hurt to run followign command twice
                    proc$.blit_complete()
                    proc$.blit_kill()
                    out_env$status[[pool_value]] <- proc$get_exit_status()

                    # release this pool
                    pools[pool] <- NA_integer_
                    cli::cli_progress_update(inc = 1L, id = progress)

                    # this pool has been released, so we directly
                    # step into next cycle and re-use this pool
                    next
                } else {
                    cli::cli_progress_update(
                        inc = 0L,
                        id = progress,
                        force = TRUE
                    )
                }
            }

            # search next pool
            if (pool == poos_size) {
                pool <- 1L
            } else {
                pool <- pool + 1L
            }
        }
        cli::cli_progress_done(id = progress)
    }

    rlang::try_fetch(
        do_parallel(),
        interrupt = function(cnd) {
            # in try_fetch, contition function are run before `on.exit()` in the
            # `expr`, so we always ensure we only kill the process, but don't
            # close the connections, so that `$.blit_complete()` method register
            # by `on.exit()` will collect all the stdout and stderr
            for (proc in out_env$process) {
                if (!is.null(proc)) {
                    proc$.blit_kill(close_connections = FALSE)
                }
            }
            cli::cli_warn(
                "System command interrupted",
                class = "system_command_interrupt"
            )
            invokeRestart("abort")
        }
    )

    for (i in seq_along(out_env$process)) {
        if (!is.null(proc <- .subset2(out_env$process, i))) {
            proc$.blit_warn_timeout(i)
        }
    }

    # merging stdout_list
    # fmt: skip
    if (rlang::is_string(stdouts) && 
        !is_processx_inherit(stdouts) && 
        !is_processx_pipe(stdouts) && 
        n > 1L) {
        if (verbose) {
            cli::cli_inform("Merging all stdouts into {.path {stdouts}}")
        }
        stdout_list <- stdout_list[file.exists(stdout_list)]
        concatenate_files(stdout_list, stdouts)
    }

    # merging stderr_list
    # fmt: skip
    if (rlang::is_string(stderrs) && 
        !is_processx_inherit(stdouts) && 
        !is_processx_pipe(stdouts) && 
        n > 1L) {
        if (verbose) {
            cli::cli_inform("Merging all stderrs into {.path {stderrs}}")
        }
        stderr_list <- stderr_list[file.exists(stderr_list)]
        concatenate_files(stderr_list, stderrs)
    }
    invisible(out_env$status)
}

# For `stdout` and `stderr`
check_stdio_list <- function(
    x,
    n,
    n_arg,
    allow_null = FALSE,
    arg = caller_arg(x),
    call = caller_call()
) {
    if (length(x) == 1L || is.null(x)) {
        x <- check_stdio(x, allow_null = allow_null, arg = arg, call = call)
        if (rlang::is_string(x) && nzchar(x) && n > 1L) {
            return(vapply(
                seq_len(n),
                function(i) {
                    tempfile(pattern = sprintf("%s_%d", arg, i))
                },
                character(1L),
                USE.NAMES = FALSE
            ))
        } else {
            return(rep_len(list(x), n))
        }
    }
    if (length(x) != n) {
        cli::cli_abort(
            paste(
                "{.arg {arg}} must be of length 1 or",
                "of the same length of {.arg {n_arg}} ({n})"
            ),
            call = call
        )
    }
    lapply(x, check_stdio, allow_null = allow_null, arg = arg, call = call)
}

check_stdin_list <- function(
    x,
    n,
    n_arg,
    arg = caller_arg(x),
    call = caller_call()
) {
    if (length(x) == 1L || is.null(x)) {
        x <- check_stdio(
            x,
            allow_bool = FALSE,
            allow_connection = FALSE,
            arg = arg,
            call = call
        )
        return(rep_len(list(x), n))
    }
    if (length(x) != n) {
        cli::cli_abort(
            paste(
                "{.arg {arg}} must be of length 1 or",
                "of the same length of {.arg {n_arg}} ({n})"
            ),
            call = call
        )
    }
    lapply(
        x,
        check_stdio,
        allow_bool = FALSE,
        allow_connection = FALSE,
        arg = arg,
        call = call
    )
}

check_callback_list <- function(
    x,
    n,
    n_arg,
    arg = caller_arg(x),
    call = caller_call()
) {
    if (length(x) == 1L || is.null(x)) {
        x <- check_callback(x, arg = arg, call = call)
        return(rep_len(list(x), n))
    }
    if (length(x) != n) {
        cli::cli_abort(
            paste(
                "{.arg {arg}} must be of length 1 or",
                "of the same length of {.arg {n_arg}} ({n})"
            ),
            call = call
        )
    }
    lapply(x, check_callback, arg = arg, call = call)
}

check_timeout_list <- function(
    x,
    n,
    n_arg,
    arg = caller_arg(x),
    call = caller_call()
) {
    if (length(x) == 1L || is.null(x)) {
        x <- check_timeout(x, arg = arg, call = call)
        return(rep_len(list(x), n))
    }
    if (length(x) != n) {
        cli::cli_abort(
            paste(
                "{.arg {arg}} must be of length 1 or",
                "of the same length of {.arg {n_arg}} ({n})"
            ),
            call = call
        )
    }
    lapply(x, check_timeout, arg = arg, call = call)
}

unwrap <- function(x) {
    if (is.list(x) && length(x) == 1L) {
        out <- .subset2(x, 1L)
        if (inherits(x, "AsIs") && !is.null(out)) {
            out <- I(out)
        }
    } else {
        out <- x
    }
    out
}

concatenate_files <- function(files, path) {
    if (length(files) == 1L && !identical(files, path)) {
        if (inherits(path, "AsIs")) {
            write_lines(read_lines(files), path = path, append = TRUE)
        } else {
            write_lines(read_lines(files), path = path)
        }
    } else {
        if (inherits(path, "AsIs")) {
            for (file in files) {
                write_lines(read_lines(file), path = path, append = TRUE)
            }
        } else {
            write_lines(read_lines(files[1L]), path = path)
            for (file in files[-1L]) {
                write_lines(read_lines(file), path = path, append = TRUE)
            }
        }
    }
}
