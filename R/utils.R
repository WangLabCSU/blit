`%||%` <- function(x, y) if (is.null(x)) y else x

is_scalar <- function(x) {
    length(x) == 1L
}

is_scalar_numeric <- function(x) {
    is_scalar(x) && is.numeric(x)
}

is_number <- function(x) is_scalar_numeric(x) && !is.na(x)

fclass <- function(x) .subset(class(x), 1L)

switch_os <- function(windows, osx, linux, call = rlang::caller_call()) {
    if (is_windows()) {
        windows
    } else {
        sysname <- Sys.info()["sysname"]
        if (sysname == "Darwin") {
            osx
        } else if (tolower(sysname) == "linux") {
            linux
        } else {
            cli::cli_abort(
                "unsupported platform {.field {sysname}}",
                call = call
            )
        }
    }
}

is_windows <- function() identical(.Platform$OS.type, "windows")

is_osx <- function() Sys.info()["sysname"] == "Darwin"

is_linux <- function() tolower(Sys.info()["sysname"]) == "linux"
