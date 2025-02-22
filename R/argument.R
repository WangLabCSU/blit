#' Deliver arguments of command
#' @param tag A string specifying argument tag, like "-i", "-o".
#' @param value Value passed to the argument.
#' @param indicator A logical value specifying whether value should be an
#'  indicator of tag. If TRUE, logical value will explain the set or unset of
#'  tag.
#' @param lgl2int A logical value indicates whether transfrom value `TRUE` to
#'  `1` or `FALSE` to `0`. If `TRUE`, format will always be set to `"%d"`.
#' @param format The format of the value, details see [`sprintf`].
#' @param sep A character string used to separate "tag" and "value", usually `"
#'  "` or `"="`.
#' @return A string.
#' @export
arg <- function(tag, value, indicator = FALSE,
                lgl2int = FALSE, format = "%s", sep = " ") {
    assert_string(tag, allow_empty = FALSE)
    assert_bool(lgl2int)
    assert_bool(indicator)
    assert_string(format, allow_empty = FALSE)
    assert_string(sep)
    arg_internal(
        tag = tag, value = value,
        indicator = indicator, lgl2int = lgl2int,
        format = format, sep = sep
    )
}

#' @keywords internal
#' @noRd
arg_internal <- function(tag, value,
                         indicator = FALSE, lgl2int = FALSE,
                         format = "%s", sep = " ",
                         allow_null = FALSE,
                         arg = rlang::caller_arg(value),
                         call = rlang::caller_call()) {
    if (is.null(value)) {
        if (allow_null) return(NULL) # styler: off
        cli::cli_abort("{.arg {arg}} cannot be {.code NULL}", call = call)
    }
    if (indicator) {
        assert_bool(value, arg = arg, call = call)
        if (value) return(tag) else return(NULL) # styler: off
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
            assert_(value, function(x) is_scalar(x) && !is.na(x),
                "scalar", arg = arg, call = call # styler: off
            )
        }
    }
    sprintf(paste(tag, format, sep = sep), value)
}
