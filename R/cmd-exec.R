#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd", FALSE)`.
#' @examples
#' cmd_run(exec("echo", "$PATH"))
#' @return A `command` object.
#' @eval rd_collect_family("command", "`command` collections")
#' @seealso
#' `r rd_seealso()`
#' @export
exec <- make_command("exec", function(cmd, ...) {
    assert_string(cmd, allow_empty = FALSE)
    Execute$new(cmd = cmd, ...)
})

Execute <- R6Class(
    "Execute",
    inherit = Command,
    private = list(
        command_name = function() .subset2(private$.core_params, "cmd"),
        object_name = function() {
            sprintf("<Execute: %s>", private$command_name())
        },
        setup_help_params = function() {
            cli::cli_abort(c(
                paste(
                    "Don't know how to show the help document for",
                    private$object_name()
                ),
                i = paste(
                    "Please manually set the help document argument with",
                    "{.code cmd_run(exec(\"{private$command_name()}\", ...))}",
                    "instead."
                )
            ))
        }
    )
)
