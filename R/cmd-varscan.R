#' VarScan is a platform-independent software tool 
#' developed at the Genome Institute at Washington University to detect variants in NGS data.
#'
#' @param subcmd Sub-Command of varscan. Details see: `r rd_help("varscan")`.
#' @param ... `r rd_dots("varscan")`.
#' @param varscan `r rd_cmd("varscan")`.
#' @seealso
#' - <https://varscan.sourceforge.net/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
varscan <- make_command(
  "varscan",
  function(
    subcmd = NULL,
    ...,
    varscan = NULL
    ) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(varscan, allow_empty = FALSE, allow_null = TRUE)
    Varscan$new(
      cmd = varscan,
      ...,
      subcmd = subcmd
    )
  }
)

Varscan <- R6Class(
  "Varscan",
  inherit = Command,
  private = list(
    alias = function() "varscan",
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