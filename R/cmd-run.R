#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_help`: Print the help document for this command.
#' - `cmd_background`: Run the command in the background. This function is
#'   provided for complete. Instead of using this function, we suggest to use 
#'  [`cmd_parallel()`] instead, which can run multiple commands in the
#'  background and ensure all process will be cleanned when process exit.
#'
#' @param command A `command` object.
#' @param stdout,stderr Specifies how the output streams of the child process
#' are handled. Possible values include:
#'
#'  - `TRUE`: Prints the child process output to the R console.
#'  - `FALSE`: Suppresses the output stream.
#'  - **string**: A file name or path to redirect the output. If a relative path
#'    is specified, it remains relative to the current working directory, even
#'    if a different directory is set using [`cmd_wd()`].
#'  - `connection`: A writable R [`connection`] object. If the connection is not
#'    [`open()`], it will be automatically opened.
#'
#' For `cmd_help()`, only a file path (string) or a `connection` can be used.
#'
#' For `cmd_background()`, only a file path (string) or a single boolean value
#' is allowed.
#'
#' When using a `connection` (if not already open) or a `string`, wrapping it
#' with [`I()`] prevents overwriting existing content.
#'
#' The `stderr` parameter also accepts `NULL`, which redirects it to the same
#' connection (i.e., pipe or file) as `stdout`.
#' @param stdin should the input be diverted? Possible values include:
#'  - `NULL`: No standard input.
#'  - **string**: A file name or path to be used as standard input.
#' @param stdout_callback,stderr_callback Possible values include:
#'  - `NULL`: no callback function.
#'  - `function`: A function invoked for each line of standard output or error.
#' Non-text (non-character) output will be ignored. The function should accept
#' two arguments: one for the standard output or error and another for the
#' running [`process`][processx::process] object.
#'
#' @param timeout Timeout in seconds. This is a limit for the elapsed time
#' running command in the separate process.
#' @param spinner Whether to show a reassuring spinner while the process
#'   is running.
#' @param verbose A single boolean value indicating whether the command
#' execution should be verbose.
#' @return
#' - `cmd_run`: Exit status invisiblely.
#' @seealso
#'  - [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#'  - [`cmd_parallel()`]
#' @export
cmd_run <- function(command, stdout = TRUE, stderr = TRUE, stdin = NULL,
                    stdout_callback = NULL, stderr_callback = NULL,
                    timeout = NULL, spinner = FALSE, verbose = TRUE) {
    assert_s3_class(command, "command")
    stdout <- check_stdio(stdout)
    stderr <- check_stdio(stderr, allow_null = TRUE)
    stdin <- check_stdin(stdin)
    stdout_callback <- check_callback(stdout_callback)
    stderr_callback <- check_callback(stderr_callback)
    timeout <- check_timeout(timeout)
    proc <- processx_command(
        command,
        help = FALSE,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        stdout_callback = stdout_callback,
        stderr_callback = stderr_callback,
        verbose = verbose
    )
    out <- proc$.blit_run(timeout, isTRUE(spinner))
    invisible(out)
}

#' @return
#' - `cmd_help`: The input `command` invisiblely.
#' @export
#' @rdname cmd_run
cmd_help <- function(command, stdout, stderr,
                     stdout_callback = NULL, stderr_callback = NULL,
                     verbose = TRUE) {
    assert_s3_class(command, "command")
    if (missing(stdout)) {
        stdout <- TRUE
    } else {
        stdout <- check_stdio(stdout, help = TRUE)
    }
    if (missing(stderr)) {
        stderr <- TRUE
    } else {
        stderr <- check_stdio(stderr, allow_null = TRUE, help = TRUE)
    }
    stdout_callback <- check_callback(stdout_callback)
    stderr_callback <- check_callback(stderr_callback)
    proc <- processx_command(
        command,
        help = TRUE,
        stdout = stdout,
        stderr = stderr,
        stdin = NULL,
        stdout_callback = stdout_callback,
        stderr_callback = stderr_callback,
        verbose = verbose
    )
    proc$.blit_run()
    invisible(command)
}

#' @return
#' - `cmd_background`: A [`process`][processx::process] object.
#' @export
#' @rdname cmd_run
cmd_background <- function(command, stdout, stderr, stdin = NULL,
                           verbose = TRUE) {
    assert_s3_class(command, "command")
    # for background process, we cannot use pipe, since if the user don't
    # read out the standard output and/or error of the pipes, the background
    # process will stop running! So we always use `stdout = ""`/`stderr = ""`
    if (missing(stdout)) {
        stdout <- FALSE
    } else {
        stdout <- check_stdio(stdout, background = TRUE)
        if (isTRUE(stdout)) {
            if (processx::is_valid_fd(1L)) {
                cli::cli_warn(
                    paste(
                        "Direct printing to the R process's stdout of a",
                        "background process will mess up the R console"
                    ),
                    .frequency = "regularly"
                )
                stdout <- ""
            } else {
                cli::cli_abort(c(
                    "No standard output stream found",
                    i = "Please set {.code stdout = FALSE}"
                ))
            }
        }
    }
    if (missing(stderr)) {
        stderr <- FALSE
    } else {
        stderr <- check_stdio(stderr, allow_null = TRUE, background = TRUE)
        if (isTRUE(stderr)) {
            if (processx::is_valid_fd(2L)) {
                cli::cli_warn(
                    paste(
                        "Direct printing to the R process's stderr of a",
                        "background process will mess up the R console"
                    ),
                    .frequency = "regularly"
                )
                stderr <- ""
            } else {
                cli::cli_abort(c(
                    "No standard error stream found",
                    i = "Please set {.code stderr = FALSE}"
                ))
            }
        }
    }
    stdin <- check_stdin(stdin)
    processx_command(
        command,
        help = FALSE,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        verbose = verbose
    )
}

# For `stdout` and `stderr`
#' @importFrom rlang caller_arg caller_call
check_stdio <- function(x, allow_null = FALSE,
                        background = FALSE, help = FALSE,
                        arg = caller_arg(x), call = caller_call()) {
    if (is.null(x)) {
        if (!allow_null) {
            cli::cli_abort(
                "{.code NULL} is not allowed in {.arg {arg}}",
                call = call
            )
        }
        return(x)
    }

    if (rlang::is_string(x)) {
        if (!nzchar(x)) {
            cli::cli_abort(
                "empty string is not allowed in {.arg {arg}}",
                call = call
            )
        }
        return(x)
    }

    if (rlang::is_bool(x)) {
        if (help) {
            cli::cli_abort(
                "boolean value is not allowed in {.arg {arg}}",
                call = call
            )
        }
        return(x)
    }

    if (inherits(x, "connection")) {
        if (background) {
            cli::cli_abort(
                "{.cls connection} is not allowed in {.arg {arg}}",
                call = call
            )
        }
        if (isOpen(x)) {
            if (!isOpen(x, "write")) {
                cli::cli_abort(c(
                    "{.cls connection} of {.arg {arg}} is not writable",
                    i = paste(
                        "You can provide a closed {.cls connection}",
                        "or a opened writable {.cls connection}"
                    )
                ), call = call)
            }
        } else if (inherits(x, "AsIs")) {
            x <- open(x, open = "a+b")
        } else {
            x <- open(x, open = "w+b")
        }
        return(x)
    }
    cli::cli_abort(
        "{.arg {arg}} cannot be a {.obj_type_friendly {x}}",
        call = call
    )
}

check_stdin <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (is.null(x)) return(x) # styler: off
    if (rlang::is_string(x)) {
        if (!nzchar(x)) {
            cli::cli_abort(
                "empty string is not allowed in {.arg {arg}}",
                call = call
            )
        }
        return(x)
    }
    cli::cli_abort(
        "{.arg {arg}} cannot be a {.obj_type_friendly {x}}",
        call = call
    )
}

check_callback <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (!is.null(x)) {
        x <- rlang::as_function(x, arg = arg, call = call)
    }
    x
}

check_timeout <- function(x, arg = caller_arg(x), call = caller_call()) {
    if (!is.null(x)) {
        x <- rlang::try_fetch(
            as.difftime(x, units = "secs"),
            error = function(cnd) {
                cli::cli_abort(
                    paste(
                        "{.arg {arg}} must be an object",
                        "which can be coercible to {.cls difftime}"
                    ),
                    parent = cnd
                )
            }
        )
    }
    x
}
