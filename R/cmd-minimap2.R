#' Run minimap2
#'
#' The `minimap2` is a tool for fast and accurate sequence alignment, especially for
#' long-read sequencing data. It can perform pairwise alignment of DNA or RNA sequences,
#' and is optimized for long-read data such as PacBio and Oxford Nanopore.
#'
#' @param fq1,fq2 A string of fastq file path. # Path to the input FASTQ files
#' @param . `r rd_dots("minimap2")`. # Use minimap2 for alignment
#' @param ofile1,ofile2 A string of path to the output SAM file. # Output path for alignment results
#' @param minimap2 `r rd_cmd("minimap2")`. # The command to run minimap2
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/lh3/minimap2>
#'
#' `r rd_seealso()`
#' @export
minimap2 - make_command(
  "minimap2",
  function(
    fq1,
    fq2 = NULL,
    ofile1,
    ofile2 = NULL,
    minimap2 = NULL
  ){
    assert_string(minimap2, allow_empty = FALSE, allow_null = TRUE) # Check that minimap2 command is valid

    Minimap2$new( # Create an R6 object for minimap2
      cmd = minimap2,
      fq1 = fq1,
      fq2 = fq2,
      ofile1 = ofile1,
      ofile2 = ofile2
    )
  }
)

Minimap2 - R6Class(
  "Minimap2",
  inherit = Command,
  private = list(
    alias = function() "minimap2", # Alias for minimap2 tool
    setup_help_params = function() " -h", # Help flag for minimap2
    setup_command_params = function(fq1, fq2, ofile1, ofile2){
      c( # Define the command parameters for minimap2
        arg0("-a", fq1), # Use -a for the query sequences
        if (!is.null(fq2)) arg0("-b", fq2) else NULL, # If second query file exists, add it
        arg0("-o", ofile1), # Output SAM file path
        if (!is.null(ofile2)) arg0("-O", ofile2) else NULL # Optional second output file
      )
    }
  )
)
