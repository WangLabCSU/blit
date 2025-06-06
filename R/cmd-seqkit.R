#' Run seqkit
#' @param subcmd Sub-Command of seqkit.
#' @param ... `r rd_dots("seqkit subcmd")`.
#' @param seqkit `r rd_cmd("seqkit")`.
#' @seealso
#' - <https://bioinf.shenwei.me/seqkit/>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
seqkit <- make_command(
    "seqkit",
    function(subcmd = NULL, ..., seqkit = NULL) {
        assert_string(subcmd, allow_empty = FALSE, allow_null = TRUE)
        assert_string(seqkit, allow_empty = FALSE, allow_null = TRUE)
        SeqKit$new(cmd = seqkit, ..., subcmd = subcmd)
    }
)

SeqKit <- R6Class(
    "SeqKit",
    inherit = Command,
    private = list(
        alias = function() "seqkit",
        setup_help_params = function() "--help",
        combine_params = function(subcmd) {
            c(subcmd, super$combine_params())
        }
    )
)
