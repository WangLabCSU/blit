#' BCFtools is a program for variant calling and manipulating files 
#' in the Variant Call Format (VCF) and its binary counterpart BCF. 
#' All commands work transparently with both VCFs and BCFs, both uncompressed and BGZF-compressed.
#'
#' @param subcmd Sub-Command of bcftools. Details see: `r rd_help("bcftools")`.
#' @param ... `r rd_dots("bcftools")`.
#' @param bcftools `r rd_cmd("bcftools")`.
#' @seealso
#' - <https://samtools.github.io/bcftools/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
bcftools <- make_command(
  "bcftools",
  function(subcmd = NULL, ..., bcftools = NULL) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(bcftools, allow_empty = FALSE, allow_null = TRUE)
    Bcftools$new(cmd = bcftools, ..., subcmd = subcmd)
  }
)

Bcftools <- R6Class(
  "Bcftools",
  inherit = Command,
  private = list(
    alias = function() "bcftools",
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
