#' Python is a programming language that lets you work quickly and integrate
#' systems more effectively.
#'
#' @param subcmd Sub-Command of samtools. Details see: `r rd_help("samtools")`.
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
    function(subcmd = NULL, ..., samtools = NULL) {
        assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
        assert_string(samtools, allow_empty = FALSE, allow_null = TRUE)
        Samtools$new(cmd = samtools, ..., subcmd = subcmd)
    }
)

Samtools <- R6Class(
    "Samtools",
    inherit = Command,
    private = list(
        alias = function() "samtools",
        setup_help_params = function() "help",
        combine_params = function(subcmd) {
            if (private$help) {
                c(super$combine_params(), subcmd)
            } else {
                c(subcmd, super$combine_params())
            }
        }
    )
)
