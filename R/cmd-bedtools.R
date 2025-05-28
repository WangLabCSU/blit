#' Run bedtools
#'
#' The "bedtools" is a powerful toolset for genome arithmetic
#' @param
#' @param ... 'r rd_dots("bedtools")'.
#' @param
#' @param bedtools 'r rd_cmd("bedtools")'.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/arq5x/bedtools2/>
#'
#' 'r rd_seealso()'
#' @export
bedtools <- make_command(
  "bedtools",
  function(

  ){
    assert_string(bedtools,allow_empty = FALSE, allow_null = TRUE)
    BEDTools$new(
      cmd = bedtools,
    )
  }
)

BEDTools <- R6Class(
  "BEDTools",
  inherit = Command,
  private = list(
    alias = function() "bedtools",
    setup_help_params = function() "--help",
    setup_command_params = function(){
      c(

      )
    }
  )
)
