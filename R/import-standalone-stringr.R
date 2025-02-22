# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-stringr.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-stringr.R
# last-updated: 2024-11-11
# license: https://unlicense.org
# ---

# when developing R package, instead of depending on `stringr`
# we prefer use the base function
#
# Note:
# 1. these functions won't check arguments
# 2. Please use `perl`, `fixed` argument to control the pattern instead of using
#    regex(), fixed() function.

# ## Changelog
# 2024-11-11:
# First release
#
# nocov start

str_which <- function(string, pattern, ...) {
    grep(pattern = pattern, x = string, ..., value = FALSE)
}

str_c <- function(..., sep = "", collapse = NULL) {
    na_values <- rowSums(is.na(do.call("cbind", list(...)))) > 0L
    if (!is.null(collapse) && any(na_values)) {
        return(NA_character_)
    }
    out <- paste(..., sep = sep, collapse = collapse)
    if (any(na_values)) out[na_values] <- NA_character_
    out
}

str_detect <- function(string, pattern, ...) {
    grepl(pattern = pattern, x = string, ...)
}

str_subset <- function(string, pattern, ...) {
    grep(pattern = pattern, x = string, ..., value = TRUE)
}

str_replace <- function(string, pattern, replacement, ...) {
    sub(pattern = pattern, replacement = replacement, x = string, ...)
}

str_remove <- function(string, pattern, ...) {
    sub(pattern = pattern, replacement = "", x = string, ...)
}

str_replace_all <- function(string, pattern, replacement, ...) {
    gsub(pattern = pattern, replacement = replacement, x = string, ...)
}

str_remove_all <- function(string, pattern, ...) {
    gsub(pattern = pattern, replacement = "", x = string, ...)
}

str_extract <- function(string, pattern, ...) {
    matches <- regexpr(pattern, string, ...)
    start <- as.vector(matches)
    end <- start + attr(matches, "match.length") - 1L
    start[start == -1L] <- NA_integer_
    substr(string, start, end)
}

str_extract_all <- function(string, pattern, ...) {
    regmatches(
        string,
        m = gregexpr(pattern = pattern, text = string, ...)
    )
}

# split string based on pattern, Only split once, Return a list of character,
# the length of every element is two
str_split_fixed <- function(string, pattern, ...) {
    regmatches(
        string,
        regexpr(pattern = pattern, text = string, ...),
        invert = TRUE
    )
}

str_split <- function(string, pattern, ...) {
    strsplit(x = string, split = pattern, ...)
}

str_match <- function(string, pattern, ...) {
    out <- regmatches(
        string,
        regexec(pattern = pattern, text = string, ...),
        invert = FALSE
    )
    out <- lapply(out, function(x) {
        if (!length(x)) "" else x
    })
    out <- do.call("rbind", out)
    out[out == ""] <- NA_character_
    out
}

str_match_all <- function(string, pattern, ...) {
    regmatches(
        string,
        gregexec(pattern = pattern, text = string, ...),
        invert = FALSE
    )
}

str_count <- function(string, pattern, ...) {
    # This information can be gleaned from gregexpr() in base A list of the same
    #  length as text each element of which is an integer vector giving all
    #  starting position of the match or −1 if there is none.
    loc <- gregexpr(pattern = pattern, text = string, ...)
    vapply(loc, function(x) sum(x > 0L), integer(1L), USE.NAMES = FALSE)
}

str_trim <- function(string, which = "both") {
    trimws(string, which = which, whitespace = "[\\h\\v]")
}

# nocov end
