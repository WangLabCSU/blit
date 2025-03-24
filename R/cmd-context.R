#' Setup the context for the command
#'
#' @describeIn cmd_wd define the working directory.
#' @inheritParams cmd_help
#' @param wd A string or `NULL` define the working directory of the command.
#' @return
#' - `cmd_wd`: The `command` object itself, with working directory updated.
#' @seealso
#' - [`cmd_run()`]/[`cmd_help()`]/[`cmd_background()`]
#' - [`cmd_parallel()`]
#' @export
cmd_wd <- function(command, wd = NULL) {
    assert_s3_class(command, "command")
    assert_string(wd, allow_empty = FALSE, allow_null = TRUE)
    command["wd"] <- list(wd)
    command
}

#' @describeIn cmd_wd define the environment variables.
#' @inheritParams cmd_wd
#' @param ... <[dynamic dots][rlang::dyn-dots]>:
#'  - `cmd_envvar`: Named character define the environment variables.
#'  - `cmd_envpath`: Unnamed character to define the `PATH`-like environment
#' variables `name`.
#'  - `cmd_on_start`: Expression to be evaluated when the command started.
#'  - `cmd_on_exit`: Expression to be evaluated when the command finished.
#'  - `cmd_on_fail`: Expression to be evaluated when the command failed.
#'  - `cmd_on_succeed`: Expression to be evaluated when the command succeeded.
#' @param action Should the new values `"replace"`, `"prefix"` or `"suffix"`
#' existing environment variables?
#' @param sep A string to separate new and old value when `action` is `"prefix"`
#' or `"suffix"`. By default, `" "` will be used.
#' @return
#' - `cmd_envvar`: The `command` object itself, with running environment
#' variable updated.
#' @export
cmd_envvar <- function(command, ..., action = "replace", sep = NULL) {
    assert_s3_class(command, "command")
    action <- rlang::arg_match0(action, c("replace", "prefix", "suffix"))
    assert_string(sep, allow_null = TRUE)
    dots <- rlang::dots_list(..., .ignore_empty = "all")
    if (!rlang::is_named2(dots)) {
        cli::cli_abort("All input in {.arg ...} must be named")
    }
    dots[vapply(dots, is.null, logical(1L), USE.NAMES = FALSE)] <- NA_character_
    if (any(lengths(dots) != 1L)) {
        cli::cli_abort(paste(
            "All input in {.arg ...} must be of length 1",
            "or {.val NULL}"
        ))
    }
    for (nm in names(dots)) {
        command$envvar[[nm]] <- envvar_add(
            new = .subset2(dots, nm),
            old = .subset2(.subset2(command, "envvar"), nm),
            action = action,
            sep = sep
        )
    }
    command
}

#' @describeIn cmd_wd define the `PATH`-like environment variables.
#' @param name A string define the PATH environment variable name. You
#' can use this to define other `PATH`-like environment variable such as
#' `PYTHONPATH`.
#' @return
#' - `cmd_envpath`: The `command` object itself, with running environment
#' variable `name` updated.
#' @importFrom rlang :=
#' @export
cmd_envpath <- function(command, ..., action = "prefix", name = "PATH") {
    assert_s3_class(command, "command")
    rlang::check_dots_unnamed()
    assert_string(name, allow_empty = FALSE)
    envpath <- rlang::dots_list(..., .ignore_empty = "all")
    envpath <- as.character(unlist(envpath, use.names = FALSE))
    if (anyNA(envpath)) {
        cli::cli_warn("Missing value will be ignored")
        envpath <- envpath[!is.na(envpath)]
    }
    if (length(envpath) == 0L) {
        cli::cli_abort("No valid path is provided in {.arg ...}")
    }
    envpath <- normalizePath(envpath, "/", mustWork = FALSE)
    envpath <- rev(envpath)
    envpath <- paste0(envpath, collapse = .Platform$path.sep)
    cmd_envvar(
        command,
        !!name := envpath, # nolint
        action = action,
        sep = .Platform$path.sep
    )
}

#' @describeIn cmd_wd define the start code of the command
#' @return
#' - `cmd_on_start`: The `command` object itself, with the start code updated.
#' @export
cmd_on_start <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_start <- c(.subset2(command, "on_start"), rlang::enquos(...))
    command
}

#' @describeIn cmd_wd define the exit code of the command
#' @return
#' - `cmd_on_exit`: The `command` object itself, with the exit code updated.
#' @export
cmd_on_exit <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_exit <- c(.subset2(command, "on_exit"), rlang::enquos(...))
    command
}

#' @describeIn cmd_wd define the failure code of the command
#' @return
#' - `cmd_on_fail`: The `command` object itself, with the failure code updated.
#' @export
cmd_on_fail <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_fail <- c(.subset2(command, "on_fail"), rlang::enquos(...))
    command
}

#' @describeIn cmd_wd define the successful code of the command
#' @return
#' - `cmd_on_succeed`: The `command` object itself, with the successful code
#'   updated.
#' @export
cmd_on_succeed <- function(command, ...) {
    assert_s3_class(command, "command")
    command$on_succeed <- c(.subset2(command, "on_succeed"), rlang::enquos(...))
    command
}

envvar_add <- function(new, old, action, sep) {
    sep <- sep %||% attr(old, "sep", exact = FALSE)
    if (!is.na(new) && !identical(action, "replace")) {
        # `NA_character_` as a holder for the environment variable
        # and will be replaced with `Sys.getenv()` when executing the command
        # See `envvar_parse()`
        old <- old %||% NA_character_
        new <- switch(action,
            prefix = c(new, old),
            suffix = c(old, new)
        )
    }
    # if we should update the `sep` string?
    if (!is.null(sep)) attr(new, "sep") <- sep
    new
}

envvar_parse <- function(envvar) {
    envs <- names(envvar)
    names(envs) <- envs
    lapply(envs, function(nm) {
        value <- .subset2(envvar, nm)
        # for single NA value, we unset this environment variable
        if (is.null(value) || (length(value) == 1L && is.na(value))) {
            value <- NA_character_
        } else {
            na <- Sys.getenv(nm, unset = NA_character_, names = FALSE)
            # By default, we use `sep = " "`
            sep <- attr(value, "sep", exact = TRUE) %||% " "
            # if the environment variable is not set
            if (is.na(na)) {
                value <- value[!is.na(value)]
            } else {
                value[is.na(value)] <- na
            }
            value <- paste(value, collapse = sep)
        }
        value
    })
}

set_envvar <- function(envs) {
    unset <- vapply(envs, is.na, logical(1L), USE.NAMES = FALSE)
    if (any(!unset)) {
        do.call("Sys.setenv", envs[!unset])
    }
    if (any(unset)) {
        Sys.unsetenv(names(envs)[unset])
    }
}
