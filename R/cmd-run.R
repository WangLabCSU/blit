#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_help`: Print the help document for this command.
#' - `cmd_background`: Run the command in the background.
#'
#' @param command A `command` object.
#' @param stdout,stderr How output streams of the child process are processed.
#' Possible values are:
#'
#'  - `TRUE`: Print the child output in R console.
#'  - `FALSE`: Suppress output stream
#'  - **string**: Name or path of file to redirect output
#'  - `connection`: A writable R [`connection`] object.
#'
#' For `cmd_help()`, only a string (file path), or `connection` can be used.
#'
#' For `cmd_background()`, only a string (file path), or a single boolean value
#' can be used.
#'
#' `stderr` can also accept `NULL`, which means  redirect it to the same
#' connection (i.e. pipe or file) as `stdout`.
#'
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
                    timeout = NULL, spinner = TRUE, verbose = TRUE) {
    assert_number_whole(timeout, allow_null = TRUE)
    stdout <- check_io(stdout)
    if (!is.null(stderr)) stderr <- check_io(stderr)
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
    proc$.blit_wait_with_interrupt(timeout, isTRUE(spinner))
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
        stdout <- check_io(stdout, help = TRUE)
    }
    if (missing(stderr)) {
        stderr <- TRUE
    } else if (!is.null(stderr)) {
        stderr <- check_io(stderr, help = TRUE)
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
    proc$.blit_wait_with_interrupt()
    invisible(command)
}

#' @return
#' - `cmd_background`: A [`process`][processx::process] object.
#' @export
#' @rdname cmd_run
cmd_background <- function(command, stdout, stderr, stdin = NULL,
                           verbose = TRUE) {
    if (missing(stdout)) {
        stdout <- FALSE
    } else {
        stdout <- check_io(stdout, background = TRUE)
        if (isTRUE(stdout)) {
            if (processx::is_valid_fd(1L)) {
                cli::cli_warn(paste(
                    "Direct printing to the R process's stdout of a",
                    "background process will mess up the R console"
                ))
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
        stderr <- check_io(stderr, background = TRUE)
        if (isTRUE(stderr)) {
            if (processx::is_valid_fd(1L)) {
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
check_io <- function(x, background = FALSE, help = FALSE,
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
        return(x)
    }
    cli::cli_abort("{.arg {arg}} cannot be a {.obj_type_friendly {x}}",
        call = call
    )
}
