#' Run bedtools
#'
#' The `bedtools` is a powerful toolset for genome arithmetic.
#' @param subcmd Sub-Command of bedtools. Details see: `r rd_help("bedtools")`.
#' @param ... `r rd_dots("bedtools")`.
#' @param bedtools `r rd_cmd("bedtools")`.
#' @seealso
#' - <http://bedtools.readthedocs.io/>
#' - <https://github.com/arq5x/bedtools2/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
bedtools <- make_command(
  "bedtools",
  function(subcmd = NULL, ..., bedtools = NULL) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(bedtools, allow_empty = FALSE, allow_null = TRUE)
    BEDTools$new(cmd = bedtools, ..., subcmd = subcmd)
  }
)

BEDTools <- R6Class(
  "BEDTools",
  inherit = Command,
  private = list(
    alias = function() "bedtools",
    setup_help_params = function() "--help",
    combine_params = function(subcmd) {
      if (private$help) {
        c(super$combine_params(), subcmd)
      } else {
        c(subcmd, super$combine_params())
      }
    }
  )
)
