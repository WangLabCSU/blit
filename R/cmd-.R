#' Helper function to create new command
#'
#' @param name A string of the function name.
#' @param fun A function used to initialize the `Command` object.
#' @param envir A environment used to bind the created function.
#' @return A function.
#' @importFrom rlang caller_env
#' @export
make_command <- function(name, fun, envir = caller_env()) {
    force(name)
    out <- rlang::new_function(
        rlang::fn_fmls(fun),
        quote({
            # capture the call, and modify it if the first unnamed value is
            # a `command`
            call <- as.list(sys.call())
            envir <- parent.frame() # the environment used to evaluate the call

            # unnamed values
            unnamed <- which(!rlang::have_name(call[-1L])) + 1L

            # prepare the ouput
            out <- NULL # should be the input `command`
            if (length(unnamed)) {
                # if the first unnamed value is a `command` object
                out <- rlang::try_fetch(
                    eval(.subset2(call, unnamed[1L]), envir = envir),
                    error = function(cnd) NULL
                )
                if (inherits(out, "command")) {
                    call <- call[-unnamed[1L]]
                } else {
                    out <- NULL
                }
            }

            # insert a new stack to save the function
            # in this way, the error message will give the function name
            new_stack <- new.env(parent = envir)
            new_stack[[name]] <- fun
            call[[1L]] <- rlang::sym(name)
            new <- eval(as.call(call), envir = new_stack)
            if (is.null(out)) {
                out <- new_command(new)
            } else {
                out$commands <- c(.subset2(out, "commands"), list(new))
            }
            out
        })
    )
    assign(name, value = out, envir = envir, inherits = FALSE)
    out
}

new_command <- function(Command, envvar = NULL, wd = NULL) {
    structure(
        list(commands = list(Command), envvar = NULL, wd = NULL),
        class = "command"
    )
}

#' @export
print.command <- function(x, ...) {
    commands <- .subset2(x, "commands")
    if (length(commands)) {
        if (length(commands) > 1L) {
            cat(
                sprintf("A sequence of %d commands:", length(commands)),
                sep = "\n"
            )
            indent <- 2L
        } else {
            indent <- 0L
        }
        for (cmd in commands) {
            print(cmd, indent = indent)
        }
    }
    if (!is.null(wd <- .subset2(x, "wd"))) {
        cat(sprintf("Working directory: %s", wd), sep = "\n")
    }
    if (!is.null(envvar <- .subset2(x, "envvar"))) {
        cat("Environment Variables:", sep = "\n")
        nms <- format(names(envvar), justify = "right")
        values <- format(envvar, justify = "left")
        cat(paste0("  ", nms, ": ", values), sep = "\n")
    }
    invisible(x)
}

#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd", FALSE)`.
#' @examples
#' cmd_run(exec("echo", "$PATH"))
#' @return A `command` object.
#' @seealso
#' - [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' - [`cmd_run()`]/[`cmd_background()`]/[`cmd_help()`]
#' @export
exec <- make_command("exec", function(cmd, ...) {
    assert_string(cmd, allow_empty = FALSE)
    Execute$new(cmd = cmd, ...)
})

Execute <- R6Class(
    "Execute",
    inherit = Command,
    public = list(
        print = function(indent = NULL) {
            name <- .subset2(private$.core_params, "cmd")
            if (!is.numeric(indent) || indent < 1L) {
                msg <- sprintf("<Command: %s>", name)
            } else {
                msg <- sprintf(
                    "%s<Command: %s>",
                    strrep(" ", as.integer(indent)),
                    name
                )
            }
            cat(msg, sep = "\n")
            invisible(self)
        }
    ),
    private = list(
        setup_help_params = function() {
            cli::cli_abort(c(
                paste(
                    "Don't know how to show the help document",
                    "for {(.subset2(private$.core_params, 'cmd'))}"
                ),
                i = paste(
                    "Please manually set the help document argument with",
                    "{.code cmd_run(exec())} instead."
                )
            ))
        }
    )
)

#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_background`: Run the command in the background.
#' - `cmd_help`: Print the help document for this command.
#' @param command A `command` object.
#' @param stdout,stderr How output streams of the child process are processed.
#' Possible values are:
#'
#'  - `TRUE`: Print the child output in R console.
#'  - `FALSE`: Suppress output stream
#'  - **string**: Name or path of file to redirect output
#'  - `connection`: A writable R [`connection`] object
#'  - `function`: A callback function (including purrr-like lambda syntax) that
#'   takes one argument, which accepts the standard output or error. You can
#'   wrap the function with [`I()`] so that it accepts raw vectors. In this
#'   case, you should use [`sys::as_text()`] to convert them into characters.
#'
#' For `cmd_background()`, only a string (file path), or a single boolean value
#' can be used.
#'
#' For `cmd_help()`, only a string (file path), `connection`, or `function` can
#' be used.
#'
#' @param stdin should the input be diverted? A character string naming a file.
#' @param timeout Timeout in seconds. This is a limit for the elapsed time
#' running command in the separate process.
#' @param verbose A single boolean value indicating whether the command
#' execution should be verbose.
#' @return
#' - `cmd_run`: Exit status invisiblely.
#' @seealso [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' @export
cmd_run <- function(command, stdout = TRUE, stderr = TRUE, stdin = NULL,
                    timeout = NULL, verbose = TRUE) {
    assert_number_whole(timeout, allow_null = TRUE)
    stdout <- check_io(stdout)
    stderr <- check_io(stderr)
    status <- cmd_exec(
        command,
        help = FALSE,
        wait = TRUE,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        timeout = timeout,
        verbose = verbose
    )
    invisible(status)
}

#' @return
#' - `cmd_background`: Returns the process ID, which can be terminated manually
#'   using [`tools::pskill()`]. You can also use [`sys::exec_status()`] to check
#'   the process's exit status.
#' @export
#' @rdname cmd_run
cmd_background <- function(command, stdout = NULL, stderr = NULL, stdin = NULL,
                           verbose = TRUE) {
    if (!is.null(stdout)) stdout <- check_io(stdout, background = TRUE)
    if (!is.null(stderr)) stderr <- check_io(stderr, background = TRUE)
    cmd_exec(
        command,
        help = FALSE,
        wait = FALSE,
        stdout = stdout %||% FALSE,
        stderr = stderr %||% FALSE,
        stdin = stdin,
        timeout = NULL,
        verbose = verbose
    )
}

#' @return
#' - `cmd_help`: the input `command` invisiblely.
#' @export
#' @rdname cmd_run
cmd_help <- function(command, stdout = NULL, stderr = NULL, verbose = TRUE) {
    if (!is.null(stdout)) stdout <- check_io(stdout, help = TRUE)
    if (!is.null(stderr)) stderr <- check_io(stderr, help = TRUE)
    cmd_exec(
        command,
        help = TRUE,
        wait = TRUE,
        stdout = stdout %||% TRUE,
        stderr = stderr %||% TRUE,
        stdin = NULL,
        timeout = NULL,
        verbose = verbose
    )
    invisible(command)
}

#' @importFrom rlang caller_call
#' @keywords internal
cmd_exec <- function(command, help = FALSE, wait = TRUE,
                     stdout = TRUE, stderr = TRUE, stdin = NULL,
                     timeout = NULL, verbose = TRUE, call = caller_call()) {
    assert_s3_class(command, "command", call = call)
    assert_string(stdin, allow_empty = FALSE, allow_null = TRUE, call = call)
    assert_bool(verbose, call = call)
    exec_command(
        command,
        help = help,
        wait = wait,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        timeout = timeout,
        verbose = verbose
    )
}

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
#' @seealso [`cmd_run()`]/[`cmd_background()`]/[`cmd_help()`]
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

#' R6 Class to prepare command parameters
#'
#' @export
Command <- R6Class("Command",
    public = list(

        #' @description Create a new `Command` object.
        #' @param ... Additional argument passed into command.
        #' @param .subcmd Sub-command string.
        initialize = function(..., .subcmd = NULL) {
            # if provided subcmd, we assign it into our object
            private$subcmd <- .subcmd

            # collect all parameters, we cannot evaluate it since if we want to
            # print help document, it's much possible there were some missing
            # argument we only evaluate necessary parameters
            input <- rlang::enquos(..., .ignore_empty = "all")

            # Extract params used
            # `command_locate`: params used to locate command path.
            # `combine_params`: combine combine dots and params or other global
            #                   params (both optional and regular params)
            # Core parameters will always be evaluated, they will be used to
            # execute the command or display the document
            core_params <- private$trim_params(c(
                rlang::fn_fmls_names(private$command_locate),
                rlang::fn_fmls_names(private$combine_params)
            ))

            # `setup_command_params`: params used to execute the command
            params <- private$trim_params(
                setdiff(
                    rlang::fn_fmls_names(private$setup_command_params),
                    core_params
                )
            )

            # there were usually additional arguments passed into command by
            # `...`, they must be un-named
            dots <- input[!rlang::names2(input) %in% c(core_params, params)]

            # here: we check if all necessary parameters have been provided by
            #       external function. (in case from myself missing provide the
            #       parameters in external function)
            missing <- setdiff(core_params, names(input))
            if (length(missing)) {
                cli::cli_abort("Missing parameters: {.arg {missing}}")
            }

            # here: we check if we need `...`.
            if (private$collect_dots) {
                # we collect and check dots
                named <- dots[rlang::have_name(dots)]
                if (length(named)) {
                    cli::cli_abort(
                        "Unknown parameter{?s}: {.arg {names(named)}}"
                    )
                }
                if (length(dots)) private$.dots <- dots
            } else if (length(dots)) {
                if (rlang::is_named(dots)) {
                    note <- paste(
                        "Did you misname argument{?s}",
                        "({.arg {names(dots)}})?"
                    )
                } else {
                    note <- "Did you forget to name an argument?"
                }
                cli::cli_abort(c("`...` must be empty", i = note))
            }

            core_params <- lapply(
                input[intersect(names(input), core_params)],
                rlang::eval_tidy
            )

            params <- input[intersect(names(input), params)]
            if (length(core_params)) private$.core_params <- core_params
            if (length(params)) private$.params <- params
        },

        #' @description Evaluate the parameters to execute command.
        #' @return The object itself.
        evaluate = function() {
            # only evaluate the parameters once
            if (is.null(private$.evaluated_params) &&
                !is.null(private$.params)) {
                private$.evaluated_params <- lapply(
                    private$.params, rlang::eval_tidy
                )
            }
            if (is.null(private$.evaluated_dots) && !is.null(private$.dots)) {
                private$.evaluated_dots <- build_command_params(
                    lapply(private$.dots, rlang::eval_tidy),
                    paste(
                        "Only objects that can be coerced into",
                        "a character vector can be input in {.code ...}"
                    )
                )
            }
            invisible(self)
        },

        #' @description Build parameters to run command.
        #' @param help A boolean value indicating whether to build parameters
        #' for help document or not.
        #' @param verbose A boolean value indicating whether the command
        #' execution should be verbose.
        #' @param envir An environment used to Execute command.
        #' @return An atomic character combine the command and parameters.
        #' @importFrom rlang caller_env
        build = function(help = FALSE, verbose = TRUE, envir = caller_env()) {
            private$envir <- envir
            private$verbose <- verbose
            core_params <- private$.core_params

            # locate command path ------------------------------
            command <- rlang::inject(private$command_locate(
                !!!core_params[intersect(
                    rlang::fn_fmls_names(private$command_locate),
                    names(core_params)
                )]
            ))
            if (is.null(command) || !nzchar(command)) {
                cli::cli_abort("Cannot locate command {.field {private$name}}")
            }

            # prepare command parameters -----------------------
            if (isTRUE(help)) {
                private$params <- build_command_params(
                    private$setup_help_params(),
                    paste(
                        "`$setup_help_params()` method must return an",
                        "object that can be coerced into a character vector."
                    )
                )
                private$dots <- character()
            } else {
                # always ensure the parameters have been computed
                self$evaluate()
                private$params <- build_command_params(
                    rlang::inject(private$setup_command_params(
                        !!!private$.evaluated_params[intersect(
                            rlang::fn_fmls_names(private$setup_command_params),
                            names(private$.evaluated_params)
                        )]
                    )),
                    paste(
                        "`$setup_command_params()` method must return an",
                        "object that can be coerced into a character vector."
                    )
                )
                private$dots <- private$.evaluated_dots %||% character()
            }
            combined <- rlang::inject(private$combine_params(
                !!!core_params[intersect(
                    rlang::fn_fmls_names(private$combine_params),
                    names(core_params)
                )]
            ))
            private$params <- NULL
            private$dots <- NULL

            # combine command, subcmd, and params -------
            enc2utf8(c(command, private$subcmd, combined))
        },

        #' @description Build parameters to run command.
        #' @param indent A single integer number giving the space of indent.
        #' @return The object itself.
        print = function(indent = NULL) {
            if (!is.numeric(indent) || indent < 1L) {
                msg <- sprintf("<Command: %s>", private$name)
            } else {
                msg <- sprintf(
                    "%s<Command: %s>",
                    strrep(" ", as.integer(indent)),
                    private$name
                )
            }
            cat(msg, sep = "\n")
            invisible(self)
        }
    ),
    private = list(
        # @field core_params A list of parameters used to define the command
        # itself.
        .core_params = list(),

        # @field dots A list of parameters used to execute the command
        .params = NULL,

        # @field dots Additional parameters used to execute the command.
        .dots = NULL,

        # the both fields saved the estimated value for the `$params` and
        # `$dots` respectively.
        .evaluated_params = NULL,
        .evaluated_dots = NULL,

        # the four fields carry the state when executating the command, and
        # will always be re-calculated before using
        envir = NULL, verbose = NULL,
        params = NULL, dots = NULL,

        # @field subcmd A character string define the subcmd argument.
        subcmd = NULL,

        # remove extra parameters used by internal
        trim_params = function(argv) setdiff(argv, private$extra_params),

        # @description Used to attach an expression to be evaluated when
        # exiting `exec_command2`.
        setup_exit = function(expr, after = TRUE, envir = private$envir) {
            defer(expr,
                envir = envir,
                priority = if (isTRUE(after)) "last" else "first"
            )
        },

        ##############################################################
        # Following fields or methods should be overrided by sub-class.
        # @field name A string of the command name.
        name = NULL,

        # @field alias A character giving the command alias.
        alias = NULL,

        # @field collect_dots A boolean value indicating whether `...` should be
        # collected and passed into command
        collect_dots = TRUE,

        # @field extra_params Additional parameters used by `Command` object
        # but shouldn't collected from user input.
        extra_params = NULL,

        # @description Method used to locate command
        #
        # @return An string of command path
        command_locate = function(cmd) {
            if (is.null(cmd <- cmd)) {
                commands <- c(private$name, private$alias)
                for (cmd in commands) {
                    if (nzchar(command <- Sys.which(cmd))) {
                        break
                    }
                }
                if (!nzchar(command)) {
                    cli::cli_abort(sprintf(
                        "Cannot locate %s command",
                        oxford_comma(sprintf("{.field %s}", commands),
                            final = "or"
                        )
                    ))
                }
            } else {
                command <- Sys.which(cmd)
            }
            command
        },

        # @description Method used to prepare parameters to run regular command
        #
        # @return An atomic character, or `NULL`.
        setup_command_params = function() NULL,

        # @description Method used to prepare parameters to display the help
        # documents. This method shouldn't have any arguments.
        #
        # @return An atomic character, or `NULL`.
        setup_help_params = function() {
            if (is.null(private$name)) {
                nm <- "<Command: %s>"
            } else {
                nm <- sprintf("<Command: %s>", private$name)
            }
            cli::cli_abort("No help document for {nm}")
        },

        # @description Method used to combine `dots` and `params`
        #
        # @return An atomic character.
        combine_params = function() c(private$dots, private$params)
    )
)

#' @return Always return a character.
#' @noRd
build_command_params <- function(params, msg) {
    if (is.null(params)) {
        character()
    } else if (is.character(params)) {
        params
    } else if (is.numeric(params) || is.logical(params)) {
        as.character(params)
    } else if (is.list(params)) {
        unlist(
            lapply(params, build_command_params, msg = msg),
            use.names = FALSE
        )
    } else {
        cli::cli_abort(msg)
    }
}

# Used to prepare command environment variables
exec_command <- function(command, help, wait,
                         stdout, stderr, stdin, timeout,
                         verbose) {
    # setting environment variables -------------
    if (length(envvar <- .subset2(command, "envvar")) > 0L) {
        if (verbose) {
            cli::cli_inform(
                "Setting environment variables: {.field {names(envvar)}}"
            )
        }
        old <- as.list(Sys.getenv(names(envvar),
            names = TRUE, unset = NA_character_
        ))
        on.exit(set_envvar(old), add = TRUE)
        set_envvar(envvar)
    }
    # for help document, we only display the last one
    if (help) {
        command$commands <- command$commands[length(command$commands)]
    }
    exec_command2(
        command,
        help = help,
        wait = wait,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        timeout = timeout,
        verbose = verbose
    )
}

# Used to prepare environmen used to clean the variables for each command
exec_command2 <- function(command, help, ..., verbose) {
    # save current environment -------------------------
    # `setup_exit` in Command object will push expression into this
    # environment
    envir <- environment()

    # use Command object to prepare command parameters -----
    params <- lapply(.subset2(command, "commands"), function(cmd) {
        cmd$build(help = help, verbose = verbose, envir = envir)
    })

    # combine command parameters -----------------------
    params <- Reduce(function(x, y) c(x, "|", y), params)

    # run command ---------------------------------------
    exec_command3(
        command = params,
        wd = .subset2(command, "wd"),
        ...,
        verbose = verbose
    )
}

# Used to the working directory, then this method call `system2` to invoke the
# command.
#' @param interpreter For windows, a string of "cmd" or "powershell", for
#' others, a string of "sh" or "bash".
#' @noRd
exec_command3 <- function(command, interpreter = NULL, wait = TRUE, wd = NULL,
                          stdout = TRUE, stderr = TRUE, stdin = NULL,
                          timeout = NULL, verbose = TRUE) {
    # prepare the command -----------------------
    script <- tempfile(pkg_nm())
    if (.Platform$OS.type == "windows") {
        # https://stackoverflow.com/questions/605686/how-to-write-a-multiline-command
        # for cmd "^"
        # for powershell "`"
        cmd <- interpreter %||% "cmd"
        content <- command
        if (length(content) > 1L) {
            content[-length(content)] <- switch(cmd,
                cmd = paste(content[-length(content)], "^"),
                powershell = paste(content[-length(content)], "`")
            )
        }
        content <- c(switch(cmd,
            cmd = sprintf("del /F %s", shQuote(script, "cmd")),
            powershell = sprintf("Remove-Item -Force -Path '%s'", script)
        ), content)
        arg <- switch(cmd,
            cmd = "/c",
            powershell = c("-ExecutionPolicy", "Bypass", "-File")
        )
        cmd <- paste(cmd, "exe", sep = ".")
    } else {
        content <- command
        if (length(content) > 1L) {
            content[-length(content)] <- paste(content[-length(content)], "\\")
        }
        content <- c(sprintf("rm -f '%s'", script), content)
        cmd <- interpreter %||% "sh" # or "bash"
        arg <- NULL
    }

    # set working directory ---------------------
    if (!is.null(wd)) {
        if (!dir.exists(wd) &&
            !dir.create(wd, showWarnings = FALSE)) {
            cli::cli_abort(
                "Cannot create working directory {.path {wd}}"
            )
        }
        if (verbose) {
            cli::cli_inform("Setting working directory: {.path {wd}")
        }
        old_wd <- getwd()
        setwd(wd)
        on.exit(setwd(old_wd), add = TRUE)
    }

    # write the content to the script ----------
    writeLines(content, script, sep = "\n")
    # ensure file has execute permission
    if (file.access(script, mode = 1L) != 0L) {
        Sys.chmod(script, "555")
    }

    if (verbose) {
        cli::cli_text(
            "Running command: {.field {paste(command, collapse = ' ')}}"
        )
        cli::cat_line()
    }

    # execute the command ----------------------
    if (wait) {
        if (is.null(timeout)) {
            timeout <- 0
        } else {
            timeout <- as.numeric(timeout)
        }
        sys::exec_wait(cmd, c(arg, script),
            std_out = stdout, std_err = stderr,
            std_in = stdin, timeout = timeout
        )
    } else {
        sys::exec_background(cmd, c(arg, script),
            std_out = stdout, std_err = stderr,
            std_in = stdin
        )
    }
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

# For `stdout` and `stderr`
#' @importFrom rlang caller_arg caller_call
check_io <- function(x, background = FALSE, help = FALSE,
                     arg = caller_arg(x), call = caller_call()) {
    if (rlang::is_string(x)) {
        if (!nzchar(x)) {
            cli::cli_abort(
                "{.arg {arg}} cannot be an empty string",
                call = call
            )
        }
        return(x)
    }

    if (rlang::is_bool(x)) {
        if (help) {
            cli::cli_abort(
                "{.arg {arg}} cannot be a single boolean value",
                call = call
            )
        }
        return(x)
    }

    if (inherits(x, "connection")) {
        if (background) {
            cli::cli_abort(
                "{.arg {arg}} cannot be a {.cls connection} object",
                call = call
            )
        }
        return(x)
    }
    if (rlang::is_formula(x)) {
        if (inherits(x, "AsIs")) {
            x <- I(rlang::as_function(x))
        } else {
            x <- rlang::as_function(x)
        }
    }
    if (is.function(x)) {
        if (background) {
            cli::cli_abort("{.arg {arg}} cannot be a function", call = call)
        }
        # let the function name the same as the argument name for better message
        assign(arg, x, envir = environment())
        if (inherits(x, "AsIs")) {
            o <- rlang::new_function(
                rlang::exprs(raw = , ... = ),
                substitute(
                    {
                        myfun(raw, ...)
                    },
                    list(myfun = rlang::sym(arg))
                )
            )
        } else {
            o <- rlang::new_function(
                rlang::exprs(raw = , ... = ),
                substitute(
                    {
                        myfun(sys::as_text(raw), ...)
                    },
                    list(myfun = rlang::sym(arg))
                )
            )
        }
        return(o)
    }
    cli::cli_abort("{.arg {arg}} cannot be a {.obj_type_friendly {x}}",
        call = call
    )
}

remove_opath <- function(opath) {
    # remove trailing backslash or slash
    opath <- path_trim(opath)
    opath <- opath[file.exists(opath)] # can also check directory
    if (length(opath) == 0L) return(NULL) # styler: off
    failed <- vapply(opath, unlink, integer(1L),
        recursive = TRUE, USE.NAMES = FALSE
    ) != 0L
    if (any(failed)) cli::cli_warn("Cannot remove {.path {opath[failed]}}")
}

build_opath <- function(odir, ofile = NULL, abs = FALSE,
                        call = rlang::caller_call()) {
    assert_string(odir,
        allow_empty = FALSE,
        arg = rlang::caller_arg(odir), call = call
    )
    assert_string(ofile,
        allow_empty = FALSE, allow_null = TRUE,
        arg = rlang::caller_arg(ofile), call = call
    )
    odir <- path_trim(odir)
    dir_create(odir)
    # whether to use absolute path
    if (abs) odir <- normalizePath(odir, winslash = "/", mustWork = TRUE)
    if (!is.null(ofile)) file_path(odir, ofile) else odir
}
