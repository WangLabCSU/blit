#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger")`.
#' @param cellranger `r rd_cmd("cellranger")`.
#' @inherit exec return
#' @seealso
#' - <https://github.com/10XGenomics/cellranger>
#' - <https://www.10xgenomics.com/support/software/cell-ranger/latest>
#' - <https://www.10xgenomics.com/support/software/cell-ranger/downloads#reference-downloads>
#'
#' `r rd_seealso()`
#' @examples
#' \dontrun{
#' fastq_dir  # 10x raw fastq files directory
#' genome_ref # Please download the transcriptome reference data
#' cellranger(
#'     "count",
#'     sprintf("--fastqs=%s", fastq_dir),
#'     sprintf("--id=%s", basename(fastq_dir)),
#'     sprintf("--sample=%s", basename(fastq_dir)),
#'     sprintf("--localcores=%s", parallel::detectCores()),
#'     sprintf("--transcriptome=%s", genome_ref),
#'     sprintf("--chemistry=%s", shQuote("threeprime")),
#'     "--nosecondary"
#' )
#' }
#' @family command
#' @export
cellranger <- make_command(
    "cellranger",
    function(subcmd = NULL, ..., cellranger = NULL) {
        assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
        assert_string(cellranger, allow_empty = FALSE, allow_null = TRUE)
        CellRanger$new(cmd = cellranger, ..., subcmd = subcmd)
    }
)

CellRanger <- R6Class(
    "CellRanger",
    inherit = Command,
    private = list(
        alias = function() "cellranger",
        setup_help_params = function() "--help",
        combine_params = function(subcmd) {
            c(subcmd, super$combine_params())
        }
    )
)
