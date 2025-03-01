#' Perl is a highly capable, feature-rich programming language with over 36
#' years of development.
#'
#' @param ... `r rd_dots("perl")`.
#' @param perl `r rd_cmd("perl")`.
#' @seealso <https://www.perl.org/>
#' @inherit exec return seealso
#' @family command
#' @export
perl <- make_command(
    "perl",
    function(..., perl = NULL) {
        assert_string(perl, allow_empty = FALSE, allow_null = TRUE)
        Perl$new(cmd = perl, ...)
    }
)

Perl <- R6Class(
    "Perl",
    inherit = Command,
    private = list(
        name = "perl",
        setup_help_params = function() "--help"
    )
)
