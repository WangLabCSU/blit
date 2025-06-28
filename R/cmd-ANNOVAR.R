#' Run ANNOVAR
#'
#' The `ANNOVAR` is a tool designed to provide ultrafast all-in-one preprocessing
#' and quality control for FastQ data.
#' @param fq1,fq2 A string of fastq file path.
#' @param ... `r rd_dots("ANNOVAR")`.
#' @param ofile1,ofile2 A string of path to the output fastq file.
#' @param ANNOVAR `r rd_cmd("ANNOVAR")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/WGLab/doc-ANNOVAR>
#'
#' `r rd_seealso()`
#' @export
ANNOVAR <- make_command(
  "ANNOVAR",
  function(
    fq1,
    ofile1,
    ...,
    fq2 = NULL,
    ofile2 = NULL,
    ANNOVAR = NULL
  ){
    assert_string(ANNOVAR, allow_empty = FALSE, allow_null = TRUE)
    ANNOVAR$new(
      cmd = ANNOVAR,
      ...,
      fq1 = fq1,
      fq2 = fq2,
      ofile1 = ofile1,
      ofile2 = ofile2
    )
  }
)

ANNOVAR <- R6Class(
  "ANNOVAR",
  inherit = Command,
  private = list(
    alias = function() "ANNOVAR",
    setup_help_params = function() "--help",
    setup_command_params = function(fq1, fq2, ofile1, ofile2){
      c(
        arg0("-i", fq1),
        if (!is.null(fq2)) arg0("-I", fq2) else NULL,
        arg0("-o", ofile1),
        if (!is.null(ofile2)) arg0("-O", ofile2) else NULL
      )
    }
  )
)
