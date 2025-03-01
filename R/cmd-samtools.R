#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param ... `r rd_dots("samtools")`.
#' @param samtools `r rd_cmd("samtools")`.
#' @seealso
#' - <https://www.htslib.org/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
samtools <- make_command(
    "samtools",
    function(..., samtools = NULL) {
        assert_string(samtools, allow_empty = FALSE, allow_null = TRUE)
        Samtools$new(cmd = samtools, ...)
    }
)

Samtools <- R6Class(
    "Samtools",
    inherit = Command,
    private = list(
        name = "samtools",
        setup_help_params = function() "--help"
    )
)
