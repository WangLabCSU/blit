#' Run minimap2
#'
#' Minimap2 is a versatile sequence alignment program that aligns DNA or mRNA sequences against a large reference database. 
#' Typical use cases include: 
#' (1) mapping PacBio or Oxford Nanopore genomic reads to the human genome; 
#' (2) finding overlaps between long reads with error rate up to ~15%; 
#' (3) splice-aware alignment of PacBio Iso-Seq or Nanopore cDNA or Direct RNA reads against a reference genome; 
#' (4) aligning Illumina single- or paired-end reads; 
#' (5) assembly-to-assembly alignment; 
#' (6) full-genome alignment between two closely related species with divergence below ~15%.
#' 
#' @param method Mapping preset: "map-ont" (single-end long reads) or "sr" (paired-end short reads).
#' @param ref Path to the reference genome FASTA file.
#' @param reads A character vector of FASTQ files used as input to minimap2.
#' @param ofile A string of path to the output SAM file.
#' @param ... `r rd_dots("minimap2")`.
#' @param minimap2 `r rd_cmd("minimap2")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/lh3/minimap2>
#'
#' `r rd_seealso()`
#' @export
minimap2 <- make_command(
  "minimap2",
  function(
    method,
    ref,
    reads,
    ofile,
    ...,
    minimap2 = NULL
  ){
    assert_string(minimap2, allow_empty = FALSE, allow_null = TRUE)
    Minimap2$new(
      cmd = minimap2,
      ...,
      method = method,
      ref = ref,
      reads = reads,
      ofile = ofile
    )
  }
)

Minimap2 <- R6Class(
  "Minimap2",
  inherit = Command,
  private = list(
    alias = function() "minimap2",
    setup_help_params = function() "--help",
    setup_command_params = function(method, ref, reads, ofile){
      c(
        if (!method %in% c("map-ont", "sr")){
          cli::cli_abort("{.arg method} must be map-ont or sr!")
        } else { arg0("-ax", method) },
        ref,
        if (length(reads) == 1L){
          reads
        } else if (length(reads) == 2L){
          reads
        } else {
          cli::cli_abort("{.arg reads} must be of length 1 or 2")
        },
        arg0(">", ofile)
      )
    }
  )
)
