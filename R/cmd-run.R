#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_help`: Print the help document for this command.
#' - `cmd_background`: Run the command in the background.
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
#' @param stdin should the input be diverted? A character string naming a file.
#' @param stdout_callback,stderr_callback `NULL`, or a function to call for
#'   every line of the standard output/error.
#' @param timeout Timeout in seconds. This is a limit for the elapsed time
#' running command in the separate process.
#' @param spinner Whether to show a reassuring spinner while the process
#'   is running.
#' @param verbose A single boolean value indicating whether the command
#' execution should be verbose.
#' @return
#' - `cmd_run`: Exit status invisiblely.
#' @seealso [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' @export
cmd_run <- function(command, stdout = TRUE, stderr = TRUE, stdin = NULL,
                    stdout_callback = NULL, stderr_callback = NULL,
                    timeout = NULL, spinner = FALSE, verbose = TRUE) {
    assert_number_whole(timeout, allow_null = TRUE)
    stdout <- check_std_io(stdout)
    if (!is.null(stderr)) stderr <- check_std_io(stderr)
    if (!is.null(timeout)) {
        timeout <- as.difftime(timeout, units = "secs")
    }
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
    if (missing(stdout)) {
        stdout <- TRUE
    } else {
        stdout <- check_std_io(stdout, help = TRUE)
    }
    if (missing(stderr)) {
        stderr <- TRUE
    } else if (!is.null(stderr)) {
        stderr <- check_std_io(stderr, help = TRUE)
    }
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
    # for background process, we cannot use pipe, since if the user don't
    # read out the standard output and/or error of the pipes, the background
    # process will stop running! So we always use `stdout = ""`/`stderr = ""`
    if (missing(stdout)) {
        stdout <- FALSE
    } else {
        stdout <- check_std_io(stdout, background = TRUE)
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
    } else if (!is.null(stderr)) {
        stderr <- check_std_io(stderr, background = TRUE)
        if (isTRUE(stderr)) {
            if (processx::is_valid_fd(2L)) {
                cli::cli_warn(paste(
                    "Direct printing to the R process's stderr of a",
                    "background process will mess up the R console"
                ))
                stderr <- ""
            } else {
                cli::cli_abort(c(
                    "No standard error stream found",
                    i = "Please set {.code stderr = FALSE}"
                ))
            }
        }
    }
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
check_std_io <- function(x, background = FALSE, help = FALSE,
                         arg = caller_arg(x), call = caller_call()) {
    if (rlang::is_string(x)) {
        if (!nzchar(x)) {
            cli::cli_abort(
                "{.arg {arg}} cannot be an empty string",
                call = call
            )
        }
        return(x)
    }

    if (rlang::is_bool(x)) {
        if (help) {
            cli::cli_abort(
                "{.arg {arg}} cannot be a single boolean value",
                call = call
            )
        }
        return(x)
    }

    if (inherits(x, "connection")) {
        if (background) {
            cli::cli_abort(
                "{.arg {arg}} cannot be a {.cls connection} object",
                call = call
            )
        }
        if (isOpen(x)) {
            if (!isOpen(x, "write")) {
                cli::cli_abort(
                    "Cannot write into the {.cls connection} {.arg {arg}}",
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
