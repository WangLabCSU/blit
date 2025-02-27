#' Run pyscenic
#'
#' @param subcmd Sub-Command of pyscenic.
#' @param ... `r rd_dots("pyscenic subcmd")`.
#' @param pyscenic `r rd_cmd("pyscenic")`.
#' @inherit exec return seealso
#' @references <https://github.com/aertslab/pySCENIC>
#' @export
pyscenic <- make_command(
    "pyscenic",
    function(subcmd = NULL, ..., pyscenic = NULL) {
        assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
        assert_string(pyscenic, allow_empty = FALSE, allow_null = TRUE)
        Pyscenic$new(cmd = pyscenic, ..., .subcmd = subcmd)
    }
)

Pyscenic <- R6Class(
    "Pyscenic",
    inherit = Command,
    private = list(
        name = "pyscenic",
        setup_help_params = function() "--help"
    )
)
