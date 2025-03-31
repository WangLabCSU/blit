#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_help`: Print the help document for this command.
#' - `cmd_background`: Run the command in the background. This function is
#'   provided for completeness. Instead of using this function, we recommend
#'   using [`cmd_parallel()`], which can run multiple commands in the background
#'   while ensuring that all processes are properly cleaned up when the process
#'   exits.
#'
#' @param command A `command` object.
#' @param stdout,stderr Specifies how the output/error streams of the child
#' process are handled. Possible values include:
#'
#'  - `FALSE`/`NULL`: Suppresses the output/error stream.
#'  - `TRUE`: Prints the child process output/error to the R console. If a
#'    standard output/error stream exists, `""` is used; otherwise, `"|"` is
#'    used.
#'  - **string**: An empty string `""` inherits the standard output/error stream
#'    from the main R process (Printing in the R console). If the main R process
#'    lacks a standard output/error stream, such as in `RGui` on Windows, an
#'    error is thrown. A string `"|"` prints to the standard output connection
#'    of R process (Using [`cat()`]). Alternative, a file name or path to
#'    redirect the output/error. If a relative path is specified, it remains
#'    relative to the current working directory, even if a different directory
#'    is set using [`cmd_wd()`].
#'  - `connection`: A writable R [`connection`] object. If the connection is not
#'    [`open()`], it will be automatically opened.
#'
#' For `stderr`, use string `"2>&1"` to redirect it to the same connection (i.e.
#' pipe or file) as `stdout`.
#'
#' For `cmd_help()`, use `FALSE`/`NULL` will do nothing, since it always want to
#' display the help document.
#'
#' For `cmd_background()`, `connection` cannot be used, and `TRUE` and `"|"`
#' will fallback to the empty string `""`.
#'
#' When using a `connection` (if not already open) or a `string`, wrapping it
#' with [`I()`] prevents overwriting existing content.
#'
#' @param stdin should the input be diverted? Possible values include:
#'  - `FALSE`/`NULL`: no standard input.
#'  - `TRUE`: If a standard input stream exists, `""` is used; otherwise, `NULL`
#'    is used.
#'  - **string**: An empty string `""` inherits the standard input stream from
#'    the main R process. If the main R process lacks a standard input stream,
#'    such as in `RGui` on Windows, an error is thrown. Alternative, a file name
#'    or path to redirect the input. If a relative path is specified, it remains
#'    relative to the current working directory, even if a different directory
#'    is set using [`cmd_wd()`].
#'
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
#'  - [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]/[`cmd_condaenv()`]
#'  - [`cmd_on_start()`]/[`cmd_on_exit()`]
#'  - [`cmd_on_succeed()`]/[`cmd_on_fail()`]
#'  - [`cmd_parallel()`]
#' @export
cmd_run <- function(
    command,
    stdout = TRUE,
    stderr = TRUE,
    stdin = TRUE,
    stdout_callback = NULL,
    stderr_callback = NULL,
    timeout = NULL,
    spinner = FALSE,
    verbose = TRUE) {
    assert_s3_class(command, "command")
    stdout <- check_stdio(stdout)
    stderr <- check_stdio(stderr)
    stdin <- check_stdio(stdin, allow_connection = FALSE)
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
    proc$.blit_run(timeout = timeout, spinner = isTRUE(spinner))
    invisible(proc$.blit_signal())
}

#' @return
#' - `cmd_help`: The input `command` invisiblely.
#' @export
#' @rdname cmd_run
cmd_help <- function(
    command,
    stdout = TRUE,
    stderr = TRUE,
    stdout_callback = NULL,
    stderr_callback = NULL,
    verbose = TRUE) {
    assert_s3_class(command, "command")
    stdout <- check_stdio(stdout)
    stderr <- check_stdio(stderr)
    stdout_callback <- check_callback(stdout_callback)
    stderr_callback <- check_callback(stderr_callback)
    proc <- processx_command(
        command,
        help = TRUE,
        stdout = stdout,
        stderr = stderr,
        stdin = TRUE,
        stdout_callback = stdout_callback,
        stderr_callback = stderr_callback,
        verbose = verbose
    )
    proc$.blit_run(spinner = FALSE)
    invisible(command)
}

#' @return
#' - `cmd_background`: A [`process`][processx::process] object.
#' @export
#' @rdname cmd_run
cmd_background <- function(
    command,
    stdout = FALSE,
    stderr = FALSE,
    stdin = NULL,
    verbose = TRUE) {
    assert_s3_class(command, "command")
    # for background process, we cannot use pipe, since if the user don't
    # read out the standard output and/or error of the pipes, the background
    # process will stop running! So we always use `stdout = ""`/`stderr = ""`
    stdout <- check_stdio(stdout, allow_connection = FALSE)
    if (isTRUE(stdout) || is_processx_pipe(stdout)) stdout <- ""
    if (is_processx_inherit(stdout)) {
        if (processx::is_valid_fd(1L)) {
            cli::cli_warn(
                paste(
                    "Direct printing to the R process's stdout of a",
                    "background process will mess up the R console"
                ),
                .frequency = "regularly",
                .frequency_id = "cmd_background_stdout"
            )
        } else {
            cli::cli_abort(c(
                "No standard output stream found",
                i = "Please set {.code stdout = FALSE}"
            ))
        }
    }
    stderr <- check_stdio(stderr, allow_connection = FALSE)
    if (isTRUE(stderr) || is_processx_pipe(stderr)) stderr <- ""
    if (is_processx_inherit(stderr)) {
        if (processx::is_valid_fd(2L)) {
            cli::cli_warn(
                paste(
                    "Direct printing to the R process's stderr of a",
                    "background process will mess up the R console"
                ),
                .frequency = "regularly",
                .frequency_id = "cmd_background_stderr"
            )
        } else {
            cli::cli_abort(c(
                "No standard error stream found",
                i = "Please set {.code stderr = FALSE}"
            ))
        }
    }
    stdin <- check_stdio(stdin, allow_connection = FALSE)
    processx_command(
        command,
        help = FALSE,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        verbose = verbose
    )
}

# https://github.com/r-lib/processx/issues/392
# For `stdout` and `stderr`
#' @importFrom rlang caller_arg caller_call
check_stdio <- function(x, allow_connection = TRUE,
                        arg = caller_arg(x), call = caller_call()) {
    if (rlang::is_bool(x) || rlang::is_string(x) || is.null(x)) {
        return(x)
    }

    if (inherits(x, "connection")) {
        if (!allow_connection) {
            cli::cli_abort(
                "{.cls connection} is not allowed in {.arg {arg}}",
                call = call
            )
        }
        if (isOpen(x)) {
            if (!isOpen(x, "write")) {
                cli::cli_abort(
                    c(
                        "{.cls connection} of {.arg {arg}} is not writable",
                        i = paste(
                            "You can provide a closed {.cls connection}",
                            "or a opened writable {.cls connection}"
                        )
                    ),
                    call = call
                )
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
        if (x <= 0L) cli::cli_abort("{.arg {arg}} must be a positive value")
    }
    x
}
