#' Run BWA
#'
#' BWA is a software package that aligns low-divergence sequences to a
#' large reference genome, such as the human genome
#' @param subcmd Sub-Command of BWA (e.g., "index", "mem").
#' @param ... `r rd_dots("bwa")`.
#' @param bwa `r rd_cmd("bwa")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <http://bio-bwa.sourceforge.net/>
#'
#' `r rd_seealso()`
#' @examples
#' \dontrun{
#' # Index reference genome
#' bwa("index", "-a", "bwtsw", "reference.fa")$run()
#'
#' # Paired-end sequence alignment
#' bwa("mem", "-t", "4", "reference.fa", "read1.fq", "read2.fq")$run(stdout = "output.sam")
#'
#' # Single-end alignment (generate sai file)
#' bwa("aln", "-t", "4", "reference.fa", "read.fq")$run(stdout = "read.sai")
#' }
#' @family command
#' @export
bwa <- make_command(
  "bwa",
  function(subcmd = NULL, ..., bwa = NULL) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(bwa, allow_empty = FALSE, allow_null = TRUE)
    BWA$new(cmd = bwa, ..., subcmd = subcmd)
  }
)

BWA <- R6Class(
  "BWA",
  inherit = Command,
  private = list(
    alias = function() "bwa",
    setup_help_params = function() "--help",
    combine_params = function(subcmd) {
      c(subcmd, super$combine_params())
    }
  )
)
