#' Schedule expressions to run
#'
#' @describeIn cmd_on_start define the startup code of the command
#' @inheritParams cmd_wd
#' @param ... The expressions input will be captured with
#' [`enquos()`][rlang::enquos]. If your expressions depend on global data, you
#' may want to unquote objects with [`!!`][rlang::injection-operator] to prevent
#' unintended changes due to delayed evaluation.
#'  - `cmd_on_start`: Expression to be evaluated when the command started.
#'  - `cmd_on_exit`: Expression to be evaluated when the command finished.
#'  - `cmd_on_fail`: Expression to be evaluated when the command failed.
#'  - `cmd_on_succeed`: Expression to be evaluated when the command succeeded.
#' @return
#' - `cmd_on_start`: The `command` object itself, with the start code updated.
#' @export
cmd_on_start <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_start <- c(.subset2(command, "on_start"), rlang::enquos(...))
    command
}

#' @describeIn cmd_on_start define the exit code of the command
#' @return
#' - `cmd_on_exit`: The `command` object itself, with the exit code updated.
#' @export
cmd_on_exit <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_exit <- c(.subset2(command, "on_exit"), rlang::enquos(...))
    command
}

#' @describeIn cmd_on_start define the failure code of the command
#' @return
#' - `cmd_on_fail`: The `command` object itself, with the failure code updated.
#' @export
cmd_on_fail <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_fail <- c(.subset2(command, "on_fail"), rlang::enquos(...))
    command
}

#' @describeIn cmd_on_start define the successful code of the command
#' @return
#' - `cmd_on_succeed`: The `command` object itself, with the successful code
#'   updated.
#' @export
cmd_on_succeed <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_succeed <- c(.subset2(command, "on_succeed"), rlang::enquos(...))
    command
}
