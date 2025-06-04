#' Run bedtools
#'
#' The `bedtools` is a powerful toolset for genome arithmetic.
#' @param subcmd Sub-Command of bedtools. Details see: `r rd_help("bedtools")`.
#' @param file1 Path to bed/gff(gtf)/vcf/bam(sam) file.
#' @param file2 Path to the second file, some commands require.
#' @param ... `r rd_dots("bedtools")`.
#' @param ofile Path to the output file.
#' @param bedtools `r rd_cmd("bedtools")`.
#' @seealso
#' - <https://github.com/arq5x/bedtools2/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
bedtools <- make_command(
  "bedtools",
  function(
    subcmd = NULL,
    file1,
    ofile,
    ...,
    file2 = NULL,
    bedtools = NULL
  ) {
    assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
    assert_string(bedtools, allow_empty = FALSE, allow_null = TRUE)
    BEDTools$new(
      cmd = bedtools,
      ...,
      subcmd = subcmd,
      file1 = file1,
      file2 = file2,
      ofile = ofile
    )
  }
)

BEDTools <- R6Class(
  "BEDTools",
  inherit = Command,
  private = list(
    alias = function() "bedtools",
    setup_help_params = function() "help",
    combine_params = function(subcmd, file1, file2, ofile) {
      c(
        if (private$help) {
          c(super$combine_params(), subcmd)
        } else {
          c(subcmd, super$combine_params())
        },
        arg0("-a", file1),
        if (!is.null(file2)) arg0("-b", file2) else NULL,
        arg0("-o", ofile),
      )
    }
  )
)
