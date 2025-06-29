#' Run bowtie2
#'
#' Bowtie 2 is an ultrafast and memory-efficient tool 
#' for aligning sequencing reads to long reference sequences. 
#' It is particularly good at aligning reads of about 50 up to 100s or 1,000s of characters, 
#' and particularly good at aligning to relatively long (e.g. mammalian) genomes. 
#' Bowtie 2 indexes the genome with an FM Index to keep its memory footprint small: 
#' for the human genome, its memory footprint is typically around 3.2 GB. 
#' Bowtie 2 supports gapped, local, and paired-end alignment modes.
#' 
#' @param index Path to bowtie2 index prefix (without file extensions).
#' @param reads A character vector of FASTQ files used as input to bowtie2.
#' @param ofile A string of path to the output sam file.
#' @param ... `r rd_dots("bowtie2")`.
#' @param bowtie2 `r rd_cmd("bowtie2")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://bowtie-bio.sourceforge.net/bowtie2/index.shtml>
#'
#' `r rd_seealso()`
#' @export
bowtie2 <- make_command(
  "bowtie2",
  function(
    index,
    reads,
    ofile,
    ...,
    bowtie2 = NULL
  ){
    assert_string(bowtie2, allow_empty = FALSE, allow_null = TRUE)
    Bowtie2$new(
      cmd = bowtie2,
      ...,
      index = index,
      reads = reads,
      ofile = ofile
    )
  }
)

Bowtie2 <- R6Class(
  "Bowtie2",
  inherit = Command,
  private = list(
    alias = function() "bowtie2",
    setup_help_params = function() "--help",
    setup_command_params = function(index, reads, ofile){
      c(
        arg0("-x", index),
        if (length(reads) == 1L){
          arg0("-U", reads)
        } else if (length(reads) == 2L){
          arg0("-1", reads[1], "-2", reads[2])
        } else {
          cli::cli_abort("{.arg reads} must be of length 1 or 2")
        },
        arg0("-S", ofile)
      )
    }
  )
)
