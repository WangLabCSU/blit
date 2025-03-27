#' Deliver arguments of command
#'
#' `arg()` is intended for user use, while `arg0()` is for developers and does
#' not perform argument validation.
#'
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param indicator A logical value specifying whether value should be an
#'  indicator of tag. If TRUE, logical value will explain the set or unset of
#'  tag.
#' @param lgl2int A logical value indicates whether transfrom value `TRUE` to
#'  `1` or `FALSE` to `0`. If `TRUE`, format will always be set to `"%d"`.
#' @param format The format of the value, details see [`sprintf`].
#' @param sep A character string used to separate `"tag"` and `"value"`, usually
#' `" "` or `"="`.
#' @return A string.
#' @export
arg <- function(tag, value, indicator = FALSE,
                lgl2int = FALSE, format = "%s", sep = " ") {
    assert_string(tag, allow_empty = FALSE)
    assert_bool(lgl2int)
    assert_bool(indicator)
    assert_string(format, allow_empty = FALSE)
    assert_string(sep)
    arg0(
        tag = tag, value = value,
        indicator = indicator, lgl2int = lgl2int,
        format = format, sep = sep
    )
}

#' @param allow_null A single logical value indicates whether `value` can be
#' `NULL`.
#' @param arg An argument name as a string. This argument will be mentioned in
#' error messages as the input that is at the origin of a problem.
#' @param call The execution environment of a currently running function.
#' @importFrom rlang caller_arg caller_call
#' @export
#' @rdname arg
arg0 <- function(tag, value,
                 indicator = FALSE, lgl2int = FALSE,
                 format = "%s", sep = " ", allow_null = FALSE,
                 arg = caller_arg(value), call = caller_call()) {
    if (is.null(value)) {
        if (allow_null) {
            return(NULL)
        }
        cli::cli_abort("{.arg {arg}} cannot be {.code NULL}", call = call)
    }
    if (indicator) {
        assert_bool(value, arg = arg, call = call)
        if (value) {
            return(tag)
        } else {
            return(NULL)
        }
    }
    assert_string(sep, call = call)
    if (lgl2int) {
        assert_bool(value, arg = arg, call = call)
        format <- "%d"
        value <- as.integer(value)
    } else {
        assert_string(format, allow_empty = FALSE, call = call)
        if (format == "%d") {
            assert_(value, is_number, "a number", arg = arg, call = call)
        } else {
            assert_(value,
                function(x) is_scalar(x) && !is.na(x),
                "a single value",
                arg = arg, call = call
            )
        }
    }
    sprintf(paste(tag, format, sep = sep), value)
}
