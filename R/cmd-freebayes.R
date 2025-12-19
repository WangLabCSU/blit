#' Run freebayes
#'
#'freebayes is a Bayesian genetic variant detector designed to find small polymorphisms, 
#'specifically SNPs (single-nucleotide polymorphisms), indels (insertions and deletions), 
#'MNPs (multi-nucleotide polymorphisms), and complex events (composite insertion and substitution events) 
#'smaller than the length of a short-read sequencing alignment.
#' @param ref A string of reference file path.
#' @param input A string of path to the input bam file.
#' @param ofile A string of path to the output vcf file.
#' @param ... `r rd_dots("freebayes")`.
#' @param freebayes `r rd_cmd("freebayes")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/freebayes/freebayes>
#'
#' `r rd_seealso()`
#' @export
freebayes <- make_command(
  "freebayes",
  function(
    ref,
    input,
    ofile,
    ...,
    freebayes = NULL
  ){
    assert_string(freebayes, allow_empty = FALSE, allow_null = TRUE)
    Freebayes$new(
      cmd = freebayes,
      ...,
      ref = ref,
      input = input,
      ofile = ofile,
    )
  }
)

Freebayes <- R6Class(
  "Freebayes",
  inherit = Command,
  private = list(
    alias = function() "freebayes",
    setup_help_params = function() "--help",
    setup_command_params = function(ref, input, ofile){
      c(
        arg0("-f", ref),
        arg0("-b", input),
        arg0("-v", ofile)
      )
    }
  )
)
