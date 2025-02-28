#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd", FALSE)`.
#' @examples
#' cmd_run(exec("echo", "$PATH"))
#' @return A `command` object.
#' @seealso
#' - [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' - [`cmd_run()`]/[`cmd_help()`]
#' @export
exec <- make_command("exec", function(cmd, ...) {
    assert_string(cmd, allow_empty = FALSE)
    Execute$new(cmd = cmd, ...)
})

Execute <- R6Class(
    "Execute",
    inherit = Command,
    public = list(
        print = function(indent = NULL) {
            name <- .subset2(private$.core_params, "cmd")
            if (!is.numeric(indent) || indent < 1L) {
                msg <- sprintf("<Command: %s>", name)
            } else {
                msg <- sprintf(
                    "%s<Command: %s>",
                    strrep(" ", as.integer(indent)),
                    name
                )
            }
            cat(msg, sep = "\n")
            invisible(self)
        }
    ),
    private = list(
        setup_help_params = function() {
            name <- .subset2(private$.core_params, "cmd")
            cli::cli_abort(c(
                "Don't know how to show the help document for {name}",
                i = paste(
                    "Please manually set the help document argument with",
                    "{.code cmd_run(exec('{name}'))} instead."
                )
            ))
        }
    )
)
