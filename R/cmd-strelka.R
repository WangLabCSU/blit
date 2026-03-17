#' Run strelka
#' 
#' Strelka is a fast and accurate small variant caller optimized for analysis of germline variation 
#' in small cohorts and somatic variation in tumor/normal sample pairs. 
#'
#' @param script Name of the Strelka script. One of
#' `r oxford_comma(code_quote(StrelkaScripts))`.
#' @param ... `r rd_dots("strelka")`.
#' @param strelka `r rd_cmd("strelka")`.
#' @seealso 
#' - <https://github.com/Illumina/strelka>
#' 
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
strelka <- make_command(
  "strelka",
  function(script, ...) {
    script <- rlang::arg_match0(script, StrelkaScripts)
    Strelka$new(
      script = script,
      ...
      )
  }
)

Strelka <- R6::R6Class(
  "Strelka",
  inherit = Command,
  private = list(
    alias = function() "strelka",
    command_locate = function() {
      py2 <- Sys.which("python2")
      if (py2 == "") {
        stop("Cannot locate python2. Please install it or activate a Conda environment with Python2.")
      }else{py2}
    },
    setup_help_params = function() "--help",
    setup_command_params = function(script) {
      script_path <- pkg_extdata("strelka", paste0(script, ".py"))
      file_executable(script_path)
      c(script_path, super$combine_params())
    }
  )
)

StrelkaScripts <- c(
  "configureStrelkaGermlineWorkflow", "configureStrelkaSomaticWorkflow", "runWorkflow"
)