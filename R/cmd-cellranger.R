#' Run cellranger
#' @param subcmd Sub-Command of cellranger.
#' @param ... `r rd_dots("cellranger subcmd")`.
#' @param cellranger `r rd_cmd("cellranger")`.
#' @inherit exec return
#' @seealso
#' `r rd_seealso()`
#' @family command
#' @export
cellranger <- make_command(
    "cellranger",
    function(subcmd = NULL, ..., cellranger = NULL) {
        assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
        assert_string(cellranger, allow_empty = FALSE, allow_null = TRUE)
        CellRanger$new(cmd = cellranger, ..., .subcmd = subcmd)
    }
)

CellRanger <- R6Class(
    "CellRanger",
    inherit = Command,
    private = list(
        name = "cellranger",
        setup_help_params = function() "--help"
    )
)
