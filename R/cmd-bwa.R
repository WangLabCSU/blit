#' Run BWA
#'
#' @param subcmd Sub-Command of BWA (e.g., "index", "mem").
#' @param ... `r rd_dots("bwa")`.
#' @param bwa `r rd_cmd("bwa")`.
#' @inherit exec return
#' @seealso
#' - <http://bio-bwa.sourceforge.net/>
#'
#' `r rd_seealso()`
#' @examples# 索引参考基因组
#bwa("index", "-a", "bwtsw", "reference.fa")$run()

# 序列比对
#bwa("mem",
#    "-t", "4"
#    "reference.fa"
#    "read1.fq"
#    "read2.fq")$run(stdout = "output.sam")
#' \dontrun{
#' # 建立索引
#' bwa("index", "-a", "bwtsw", "reference.fa")$run()
#'
#' # 双端序列比对
#' bwa("mem", "-t", "4", "reference.fa", "read1.fq", "read2.fq")$run(stdout = "output.sam")
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
