# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-pkg.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-pkg.R
# last-updated: 2025-03-03
# license: https://unlicense.org
# imports: [utils]
# ---

# This file contains several helper functions for package checking,
# installation, and other utilities.

# ## Changelog
# 2025-03-03:
# - Add `oxford_and`
# - Add `oxford_or`
# - Add `code_quote`
# - Add `oxford_comma`
#
# 2025-02-26:
# - Add `is_installed`
# - Add `install_pkgs`
# - Add `pkg_nm`
# - Add `pkg_namespace`
#
# nocov start

is_installed <- local({
    cache <- new.env(parent = emptyenv())
    function(pkg, version = NULL) {
        id <- if (is.null(version)) pkg else paste(pkg, version, sep = ":")
        out <- cache[[id]]
        if (is.null(out)) {
            if (is.null(version)) {
                out <- requireNamespace(pkg, quietly = TRUE)
            } else {
                out <- requireNamespace(pkg, quietly = TRUE) &&
                    utils::packageVersion(pkg) >= version
            }
            assign(id, out, envir = cache, inherits = FALSE)
        }
        out
    }
})

install_pkgs <- function(pkgs) {
    if (is_installed("pak")) {
        getExportedValue("pak", "pkg_install")(pkgs, ask = FALSE)
    } else {
        utils::install.packages(pkgs)
    }
}

pkg_nm <- function() utils::packageName(environment())

pkg_namespace <- function() topenv(environment())

set_exit <- function(expr, envir = parent.frame(), after = TRUE, add = TRUE) {
    expr <- getExportedValue("rlang", "enquo")(expr)
    thunk <- as.call(list(
        getExportedValue("rlang", "new_function")(list(), expr)
    ))
    do.call(base::on.exit, list(thunk, add = add, after = after), envir = envir)
}

# utils function to collapse characters ---------------------------
oxford_and <- function(x, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(x, code, quote), sep = sep, final = "and")
}

oxford_or <- function(x, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(x, code, quote), sep = sep, final = "or")
}

code_quote <- function(x, code = TRUE, quote = TRUE) {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    x
}

oxford_comma <- function(x, sep = ", ", final = "and") {
    n <- length(x)

    if (n < 2L) return(x) # styler: off

    head <- x[seq_len(n - 1L)]
    last <- x[n]

    head <- paste(head, collapse = sep)

    # Write a or b. But a, b, or c.
    if (n > 2L) {
        paste0(head, sep, final, " ", last)
    } else {
        paste0(head, " ", final, " ", last)
    }
}

# nocov end
