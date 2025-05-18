#' Run fastp
#'
#' The `fastp` is a tool designed to provide ultrafast all-in-one preprocessing
#' and quality control for FastQ data.
#' @param fq1,fq2 A string of fastq file path.
#' @param ofile1,ofile2 A string of path to the output fastq file.
#' @param ... `r rd_dots("fastp")`.
#' @param fastp `r rd_cmd("fastp")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/OpenGene/fastp>
#'
#' `r rd_seealso()`
#' @export
fastp <- make_command(
  "fastp",
  function(
    fq1,
    ofile1,
    ...,
    fq2 = NULL,
    ofile2 = NULL,
    fastp = NULL
  ){
    assert_string(fastp, allow_empty = FALSE, allow_null = TRUE)
    Fastp$new(
      cmd = fastp,
      ...,
      fq1 = fq1,
      fq2 = fq2,
      ofile1 = ofile1,
      ofile2 = ofile2
    )
  }
)

Fastp <- R6Class(
  "Fastp",
  inherit = Command,
  private = list(
    alias = function() "fastp",
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
