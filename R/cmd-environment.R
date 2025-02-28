#' Define the environment when running the command
#'
#' - `cmd_wd`: define the working directory.
#' - `cmd_envvar`: define the environment variables.
#' - `cmd_envpath`: define the `PATH`-like environment variables.
#' @inheritParams cmd_help
#' @param wd A string or `NULL` define the working directory of the command.
#' @return
#' - `cmd_wd`: The `command` object itself, with working directory updated.
#' - `cmd_envvar`: The `command` object itself, with running environment
#' variable updated.
#' - `cmd_envpath`: The `command` object self, with running environment variable
#' `name` updated.
#' @seealso [`cmd_run()`]/[`cmd_help()`]
#' @export
cmd_wd <- function(command, wd = NULL) {
    assert_s3_class(command, "command")
    assert_string(wd, allow_empty = FALSE, allow_null = TRUE)
    command["wd"] <- list(wd)
    command
}

#' @inheritParams cmd_wd
#' @param ...
#'  - `cmd_envvar`: Named character define the environment variables.
#'  - `cmd_envpath`: Unnamed character to define the `PATH`-like environment
#' variables `name`.
#' @param action Should the new values `"replace"`, `"prefix"` or `"suffix"`
#' existing environment variables?
#' @param sep A string to separate new and old value when `action` is `"prefix"`
#' or `"suffix"`.
#' @export
#' @rdname cmd_wd
cmd_envvar <- function(command, ..., action = "replace", sep = " ") {
    assert_s3_class(command, "command")
    action <- rlang::arg_match0(action, c("replace", "prefix", "suffix"))
    assert_string(sep)
    dots <- rlang::dots_list(..., .ignore_empty = "all")
    if (!rlang::is_named2(dots)) {
        cli::cli_abort("All elements in {.arg ...} must be named")
    }
    dots[vapply(dots, is.null, logical(1L))] <- NA_character_
    if (any(lengths(dots) != 1L)) {
        cli::cli_abort(paste(
            "all value in {.arg ...} must be of length 1",
            "or {.val NULL}"
        ))
    }
    for (nm in names(dots)) {
        command$envvar[[nm]] <- parse_envvar(
            name = nm,
            old = command$envvar[[nm]],
            new = dots[[nm]],
            action = action,
            sep = sep
        )
    }
    command
}

#' @param name A string define the PATH environment variable name. You
#' can use this to define other `PATH`-like environment variable such as
#' `PYTHONPATH`.
#' @importFrom rlang :=
#' @export
#' @rdname cmd_wd
cmd_envpath <- function(command, ..., action = "prefix", name = "PATH") {
    assert_s3_class(command, "command")
    rlang::check_dots_unnamed()
    assert_string(name, allow_empty = FALSE)
    envpath <- rlang::dots_list(..., .ignore_empty = "all")
    envpath <- unlist(envpath, use.names = FALSE)
    envpath <- as.character(envpath)
    if (anyNA(envpath)) {
        cli::cli_warn("Missing value will be ignored")
        envpath <- envpath[!is.na(envpath)]
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

set_envvar <- function(envs) {
    unset <- vapply(envs, is.na, logical(1L))
    if (any(!unset)) {
        do.call("Sys.setenv", envs[!unset])
    }
    if (any(unset)) {
        Sys.unsetenv(names(envs)[unset])
    }
}

parse_envvar <- function(name, new, old, action, sep) {
    if (!is.na(new)) {
        if (is.null(old)) {
            old <- Sys.getenv(name, unset = NA_character_, names = FALSE)
        }
        if (!is.na(old)) {
            if (action == "prefix") {
                new <- paste(new, old, sep = sep)
            } else if (action == "suffix") {
                new <- paste(old, new, sep = sep)
            }
        }
    }
    new
}
