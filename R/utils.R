`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

is_number <- function(x) is_scalar_numeric(x) && !is.na(x)

pkg_nm <- function() utils::packageName(topenv(environment()))

# mimic rlang::exprs, but will omit `{`
exprs <- function(...) {
    dots <- rlang::exprs(...)
    if (...length() == 1L && identical(dots[[1L]][[1L]], as.symbol("{"))) {
        dots <- as.list(dots[[1L]][-1L])
    }
    dots
}

fclass <- function(x) .subset(class(x), 1L)

# utils function to collapse characters ---------------------------
oxford_and <- function(chr, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(chr, code, quote), sep = sep, final = "and")
}

oxford_or <- function(chr, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(chr, code, quote), sep = sep, final = "or")
}

code_quote <- function(x, code = TRUE, quote = TRUE) {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    x
}

oxford_comma <- function(chr, sep = ", ", final = "and") {
    n <- length(chr)

    if (n < 2L) {
        return(chr)
    }

    head <- chr[seq_len(n - 1L)]
    last <- chr[n]

    head <- paste(head, collapse = sep)

    # Write a or b. But a, b, or c.
    if (n > 2L) {
        paste0(head, sep, final, " ", last)
    } else {
        paste0(head, " ", final, " ", last)
    }
}
