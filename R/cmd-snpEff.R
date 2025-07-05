#' Genetic variant annotation, and functional effect prediction toolbox. 
#' It annotates and predicts the effects of genetic variants on genes and proteins 
#' (such as amino acid changes).
#'
#' @param subcmd Sub-Command of snpEff. Details see: `r rd_help("snpEff")`.
#' @param ... `r rd_dots("snpEff")`.
#' @param snpEff `r rd_cmd("snpEff")`.
#' @seealso
#' - <https://pcingola.github.io/SnpEff/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
snpEff <- make_command(
  "snpEff",
  function(subcmd = NULL, ..., snpEff = NULL) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(snpEff, allow_empty = FALSE, allow_null = TRUE)
    SnpEff$new(cmd = snpEff, ..., subcmd = subcmd)
  }
)

SnpEff <- R6Class(
  "SnpEff",
  inherit = Command,
  private = list(
    alias = function() "snpEff",
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
