#' Run fastp
#'
#' The `fastp` is a tool designed to provide ultrafast all-in-one preprocessing
#' and quality control for FastQ data.
#' @param in1,in2 A string of fastq file path.
#' @param out1,out2 A string of path to the output fastq file.
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
    in1,
    out1,
    in2 = NULL,
    out2 = NULL,
    ...,
    fastp = NULL
  ){
    assert_string(fastp, allow_empty = FALSE, allow_null = TRUE)
    Fastp$new(
      cmd = fastp,
      ...,
      in1 = in1,
      in2 = in2,
      out1 = out1,
      out2 = out2
    )
  }
)

Fastp <- R6Class(
  "Fastp",
  inherit = Command,
  private = list(
    alias = function() "fastp",
    setup_help_params = function() "--help",
    setup_command_params = function(in1, in2, out1, out2){
      c(
        arg0("-i", in1),
        if (!is.null(in2)) arg0("-I", in2) else NULL,
        arg0("-o", out1),
        if (!is.null(out2)) arg0("-O", out2) else NULL
      )
    }
  )
)
