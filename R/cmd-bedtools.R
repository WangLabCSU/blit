#' Run bedtools
#'
#' The `bedtools` is a powerful toolset for genome arithmetic,
#' which includes `intersect`,`merge`,`coverage`,`getfasta`,`closest`,etc.
#' @param file1 Path to bed/gff(gtf)/vcf/bam(sam) file.
#' @param file2 Path to the second file, some commands require.
#' @param ...
#'  - `bedtools intersect`: `r rd_dots("bedtools intersect")`.
#'  - `bedtools merge`: `r rd_dots("bedtools merge")`.
#'  - `bedtools coverage`: `r rd_dots("bedtools coverage")`.
#'  - `bedtools getfasta`: `r rd_dots("bedtools getfasta")`.
#'  - `bedtools closest`: `r rd_dots("bedtools closest")`.
#' @param ofile Path to the output file.
#' @param
#'  - `bedtools intersect`: `r rd_cmd("bedtools intersect")`.
#'  - `bedtools merge`: `r rd_cmd("bedtools merge")`.
#'  - `bedtools coverage`: `r rd_cmd("bedtools coverage")`.
#'  - `bedtools getfasta`: `r rd_cmd("bedtools getfasta")`.
#'  - `bedtools closest`: `r rd_cmd("bedtools closest")`.
#' @seealso
#' - <https://github.com/arq5x/bedtools2/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export

#  bedtools intersect ----
`bedtools intersect` <- make_command(
  "bedtools intersect",
  function(
    file1,
    file2,
    ofile,
    ...,
    `bedtools intersect` = NULL
  ){
    assert_string(`bedtools intersect`, allow_empty = FALSE, allow_null = TRUE)
    `BEDTools Intersect`$new(
      cmd = `bedtools intersect`,
      ...,
      file1 = file1,
      file2 = file2,
      ofile = ofile,
    )
  }
)

`BEDTools Intersect` <- R6Class(
  "BEDTools Intersect",
  inherit = Command,
  private = list(
    alias = function() "bedtools intersect",
    setup_help_params = function() "--help",
    setup_command_params = function(file1, file2, ofile){
      c(
        arg0("-a", file1),
        arg0("-b", file2),
        arg0("-wa"),
        arg0("-o", ofile),
      )
    }
  )
)

#  bedtools merge ----
`bedtools merge` <- make_command(
  "bedtools merge",
  function(
    file1,
    ofile,
    ...,
    dist = NULL,
    `bedtools merge` = NULL
  ){
    assert_string(`bedtools merge`, allow_empty = FALSE, allow_null = TRUE)
    `BEDTools Merge`$new(
      cmd = `bedtools merge`,
      ...,
      file1 = file1,
      dist = dist,
      ofile = ofile,
    )
  }
)

`BEDTools Merge` <- R6Class(
  "BEDTools Merge",
  inherit = Command,
  private = list(
    alias = function() "bedtools merge",
    setup_help_params = function() "--help",
    setup_command_params = function(file1, dist, ofile){
      c(
        if (endsWith(file1, ".bed")|endsWith(file1, ".gff")){
          arg0("-i", file1)
        } else{
          cli::cli_abort(
            "{.arg file1} must be BED/GFF"
          )
        },
        if (!is.null(dist)) arg0("-d", dist) else NULL,
        arg0("-o", ofile),
      )
    }
  )
)

#  bedtools coverage ----
`bedtools coverage` <- make_command(
  "bedtools coverage",
  function(
    file1,
    file2,
    ofile,
    ...,
    `bedtools coverage` = NULL
  ){
    assert_string(`bedtools coverage`, allow_empty = FALSE, allow_null = TRUE)
    `BEDTools Coverage`$new(
      cmd = `bedtools coverage`,
      ...,
      file1 = file1,
      file2 = file2,
      ofile = ofile,
    )
  }
)

`BEDTools Coverage` <- R6Class(
  "BEDTools Coverage",
  inherit = Command,
  private = list(
    alias = function() "bedtools coverage",
    setup_help_params = function() "--help",
    setup_command_params = function(file1, file2, ofile){
      c(
        if (endsWith(file1, ".bed")|endsWith(file1, ".gff")){
          arg0("-a", file1)
        } else{
          cli::cli_abort(
            "{.arg file1} must be BED/GFF"
          )
        },
        if (endsWith(file2, ".bed")|endsWith(file2, ".gff")|endsWith(file2, ".bam")){
          arg0("-b", file2)
        } else{
          cli::cli_abort(
            "{.arg file2} must be BED/GFF/BAM"
          )
        },
        arg0("-o", ofile),
      )
    }
  )
)

#  bedtools getfasta ----
`bedtools getfasta` <- make_command(
  "bedtools getfasta",
  function(
    file1,
    file2,
    ofile,
    ...,
    `bedtools getfasta` = NULL
  ){
    assert_string(`bedtools getfasta`, allow_empty = FALSE, allow_null = TRUE)
    `BEDTools Getfasta`$new(
      cmd = `bedtools getfasta`,
      ...,
      file1 = file1,
      file2 = file2,
      ofile = ofile,
    )
  }
)

`BEDTools Getfasta` <- R6Class(
  "BEDTools Getfasta",
  inherit = Command,
  private = list(
    alias = function() "bedtools getfasta",
    setup_help_params = function() "--help",
    setup_command_params = function(file1, file2, ofile){
      c(
        if (endsWith(file1, ".fa")){
          arg0("-fi", file1)
          } else{
            cli::cli_abort(
              "{.arg file1} must be FASTA"
            )
          },
        if (endsWith(file2, ".bed")|endsWith(file2, ".gff")){
          arg0("-bed", file2)
        } else{
          cli::cli_abort(
            "{.arg file2} must be BED/GFF"
          )
        },
        arg0("-fo", ofile),
      )
    }
  )
)

#  bedtools closest ----
`bedtools closest` <- make_command(
  "bedtools closest",
  function(
    file1,
    file2,
    ofile,
    ...,
    `bedtools closest` = NULL
  ){
    assert_string(`bedtools closest`, allow_empty = FALSE, allow_null = TRUE)
    `BEDTools Closest`$new(
      cmd = `bedtools closest`,
      ...,
      file1 = file1,
      file2 = file2,
      ofile = ofile,
    )
  }
)

`BEDTools Closest` <- R6Class(
  "BEDTools Closest",
  inherit = Command,
  private = list(
    alias = function() "bedtools closest",
    setup_help_params = function() "--help",
    setup_command_params = function(file1, file2, ofile){
      c(
        if (endsWith(file1, ".bed")|endsWith(file1, ".gff")|endsWith(file1, ".vcf")){
          arg0("-a", file1)
        } else{
          cli::cli_abort(
            "{.arg file1} must be BED/GFF/VCF"
          )
        },
        if (endsWith(file2, ".bed")|endsWith(file2, ".gff")|endsWith(file2, ".vcf")){
          arg0("-b", file2)
        } else{
          cli::cli_abort(
            "{.arg file2} must be BED/GFF/VCF"
          )
        },
        args("-D"),
        arg0("-o", ofile),
      )
    }
  )
)
