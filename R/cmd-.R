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
    invisible(x)
}

#' Invoke a System Command
#'
#' @param cmd Command to be invoked, as a character string.
#' @param ... `r rd_dots("cmd", FALSE)`.
#' @examples
#' exec("echo", "$PATH") |> cmd_run()
#' @return A `command` object.
#' @seealso
#' - [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' - [`cmd_run()`]/[`cmd_help()`]
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
            name <- rlang::eval_tidy(.subset2(private$params, "cmd"))
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
    )
)

#' Execute command
#'
#' - `cmd_run`: Run the command.
#' - `cmd_help`: Print the help document for this command.
#' @param command A `command` object.
#' @param stdout,stderr Where output to 'stdout' or 'stderr' should be
#' sent. Possible values are:
#'
#'  - `NULL`: print child output in R console
#'  - `TRUE`: capture the output in a character vector
#'  - `FALSE`: suppress output stream
#'  - **string**: name or path of file to redirect output
#'
#' @param stdin should input be diverted? `NULL` means the default,
#' alternatively a character string naming a file. Ignored if input is
#' supplied.
#' @param timeout Timeout in seconds, ignored if `NULL`. This is a limit for
#' the elapsed time running command in a separate process. Fractions of
#' seconds are ignored.
#' @param verbose A single boolean value indicates whether to print running
#' command message.
#' @return
#' - `cmd_run`: Exit status or the captured output when `stdout`/`stderr` is
#'   `TRUE`.
#' @export
cmd_run <- function(command, stdout = NULL, stderr = NULL, stdin = NULL,
                    timeout = NULL, verbose = TRUE) {
    assert_s3_class(command, "command")
    assert_io(stdout)
    assert_io(stderr)
    assert_bool(verbose)
    assert_string(stdin, allow_empty = FALSE, allow_null = TRUE)
    assert_number_whole(timeout, allow_null = TRUE)
    exec_command(
        command,
        help = FALSE,
        stdout = stdout,
        stderr = stderr,
        stdin = stdin,
        timeout = timeout,
        verbose = verbose
    )
}

#' @return
#' - `cmd_help`: the input `command` or the captured output when
#'   `stdout`/`stderr` is `TRUE`.
#' @seealso [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]
#' @export
#' @rdname cmd_run
cmd_help <- function(command, stdout = NULL, stderr = NULL, verbose = TRUE) {
    assert_s3_class(command, "command")
    assert_io(stdout)
    assert_io(stderr)
    assert_bool(verbose)
    out <- exec_command(
        command,
        help = TRUE,
        stdout = stdout,
        stderr = stderr,
        stdin = "",
        timeout = 0L,
        verbose = verbose
    )
    if (isTRUE(stdout) || isTRUE(stderr)) return(out) # styler: off
    invisible(command)
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
#' @param sep A string to separate new and old value.
#' @export
#' @rdname cmd_wd
cmd_envvar <- function(command, ..., action = "replace", sep = " ") {
    assert_s3_class(command, "command")
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
    action <- rlang::arg_match0(action, c("replace", "prefix", "suffix"))
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
    if (anyNA(envpath)) {
        if (length(ignored <- envpath[!is.na(envpath)])) { # nolint
            cli::cli_warn(paste(
                "Found {.val NA}",
                "{.field {ignored}} will be ignored",
                sep = ", "
            ))
        }
        envpath <- NA_character_
    } else {
        envpath <- as.character(envpath)
        envpath <- normalizePath(envpath, "/", mustWork = FALSE)
        envpath <- paste0(envpath, collapse = .Platform$path.sep)
    }
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
            params <- rlang::enquos(..., .ignore_empty = "all")

            # Extract params used by `Command` object internal
            # `command_locate`: params used to locate command path.
            # `setup_command_params`: params used by regular command
            # `combine_params`: combine combine dots and params or other global
            #                   params (both optional and regular params)
            param_names <- private$parameters()

            # there were usually additional arguments passed into command by
            # `...`, they must be un-named
            dots <- params[!rlang::names2(params) %in% param_names]
            if (private$collect_dots) {
                # we collect and check dots
                named <- dots[rlang::have_name(dots)]
                if (length(named)) {
                    cli::cli_abort(
                        "Unknown parameter{?s}: {.arg {names(named)}}"
                    )
                }
                private$dots <- dots
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

            # here: we check if all necessary parameters have been provided by
            #       external function. (in case from myself missing provide the
            #       parameters in external function)
            missing <- setdiff(param_names, names(params))
            if (length(missing)) {
                cli::cli_abort("Missing parameters: {.arg {missing}}")
            }
            private$params <- params[intersect(names(params), param_names)]
        },

        #' @description Build parameters to run command.
        #' @param help A bool, indicates whether to build parameters for help
        #' document or not.
        #' @param envir An environment used to Execute command.
        #' @return An atomic character combine the command and parameters.
        #' @importFrom rlang caller_env
        build = function(help = FALSE, envir = caller_env()) {
            private$environment <- envir
            necessary_params <- .subset(
                private$params, private$parameters(help = help)
            )

            # weevaluate params since all params have been defused
            necessary_params <- lapply(necessary_params, rlang::eval_tidy)

            # locate command path ------------------------------
            command <- rlang::inject(private$command_locate(
                !!!necessary_params[intersect(
                    rlang::fn_fmls_names(private$command_locate),
                    names(necessary_params)
                )]
            ))
            if (is.null(command) || !nzchar(command)) {
                cli::cli_abort("Cannot locate command {.field {private$name}}")
            }

            # prepare command parameters -----------------------
            if (isTRUE(help)) {
                private$.params <- build_command_params(
                    private$setup_help_params()
                )
            } else {
                # compute command params
                private$.params <- build_command_params(
                    rlang::inject(private$setup_command_params(
                        !!!necessary_params[intersect(
                            rlang::fn_fmls_names(private$setup_command_params),
                            names(necessary_params)
                        )]
                    ))
                )
                private$.dots <- build_command_params(
                    lapply(private$dots, rlang::eval_tidy)
                )
            }
            command_params <- rlang::inject(private$combine_params(
                !!!necessary_params[intersect(
                    rlang::fn_fmls_names(private$combine_params),
                    names(necessary_params)
                )]
            ))

            # combine command, subcmd, and params -------
            c(command, private$subcmd, command_params)
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
        # @field dots A list of parameters used by command.
        dots = list(),

        # @field params A list of parameters used by `Command` object methods.
        params = list(),

        # These three fields carry state when executating the command but will
        # always be estimated before use
        environment = NULL,
        .dots = character(),
        .params = character(),

        # @field subcmd A character string define the subcmd argument.
        subcmd = NULL,

        # @description Extract parameters used by this object.
        # @return A character of the used parameter of this object.
        parameters = function(help = FALSE) {
            argv <- c(
                rlang::fn_fmls_names(private$command_locate),
                rlang::fn_fmls_names(private$combine_params)
            )
            if (!isTRUE(help)) {
                argv <- c(
                    argv,
                    rlang::fn_fmls_names(private$setup_command_params)
                )
            }
            # remove arguments used by internal only
            setdiff(argv, private$internal_params)
        },

        # @description methods to insert into or get parameters from
        # `private$params`
        set_param = function(name, value) {
            private$params[[name]] <- value
            invisible(self)
        },
        get_param = function(name) {
            out <- private$params
            if (!is.null(name)) out <- .subset2(out, name)
            out
        },

        # @description Used to attach an expression to be evaluated when
        # exiting `Execute$exec_command2`.
        setup_exit = function(expr, after = TRUE, add = TRUE,
                              envir = private$environment) {
            defer(expr,
                envir = envir,
                priority = if (isTRUE(after)) "last" else "first"
            )
        },

        ##############################################################
        # Following fields or methods should be overrided by sub-class.
        # @field A string of the command name.
        name = NULL,

        # @field A character giving the command alias.
        alias = NULL,

        # @field collect_dots A bool indicates whether `...` should be
        # collected and passed into command
        collect_dots = TRUE,

        # @field internal_params Additional parameters used by `Command` object
        # but shouldn't collected from user input.
        internal_params = NULL,

        # @description Method used to locate command
        #
        # @return An string of command path
        command_locate = function(cmd) {
            if (is.null(cmd <- rlang::eval_tidy(cmd))) {
                commands <- c(private$name, private$alias)
                for (cmd in commands) {
                    if (nzchar(command <- Sys.which(cmd))) {
                        break
                    }
                }
                if (!nzchar(command)) {
                    cli::cli_abort(sprintf(
                        "Cannot locate {.field %s} command",
                        oxford_comma(
                            sprintf("{.field %s}", commands),
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
        setup_help_params = function() NULL,

        # @description Method used to combine `dots` and `params`
        #
        # @return An atomic character, or `NULL`.
        combine_params = function() c(private$.dots, private$.params)
    )
)

#' @return Always return a character.
#' @noRd
build_command_params <- function(params) {
    if (is.character(params)) return(params) # styler: off
    if (is.null(params)) return(character()) # styler: off
    if (is.list(params)) {
        return(as.character(unlist(params, use.names = FALSE)))
    }
    cli::cli_abort("Unsupported command parameters")
}

# Used to prepare command environment variables
exec_command <- function(command, help,
                         stdout, stderr, stdin, timeout,
                         verbose) {
    # setting environment variables -------------
    if (length(envvar <- .subset2(command, "envvar")) > 0L) {
        if (verbose) {
            cli::cli_inform("Setting environment variables: {names(envvar)}")
        }
        old <- as.list(Sys.getenv(names(envvar),
            names = TRUE, unset = NA_character_
        ))
        on.exit(set_envvar(old), add = TRUE)
        set_envvar(envvar)
    }
    if (help) {
        command$commands <- command$commands[length(command$commands)]
        exec_command2(
            command,
            help = TRUE,
            stdout = stdout,
            stderr = stderr,
            stdin = "",
            timeout = 0L,
            verbose = verbose
        )
    } else {
        exec_command2(
            command,
            help = FALSE,
            stdout = stdout,
            stderr = stderr,
            stdin = stdin,
            timeout = timeout,
            verbose = verbose
        )
    }
}

# Used to prepare environmen used to clean the variables for each command
exec_command2 <- function(command, help, ...) {
    # save current environment -------------------------
    # `setup_exit` in Command object will push expression into this
    # environment
    envir <- environment()

    # use Command object to prepare command parameters -----
    params <- lapply(.subset2(command, "commands"), function(cmd) {
        cmd$build(help = help, envir = envir)
    })

    # combine command parameters -----------------------
    params <- Reduce(function(x, y) c(x, "|", y), params)

    # run command ---------------------------------------
    exec_command3(
        command = params[1L], command_params = params[-1L],
        wd = .subset2(command, "wd"), ...
    )
}

# Used to the working directory, then this method call `system2` to invoke the
# command.
exec_command3 <- function(command, command_params, wd = NULL,
                          stdout = TRUE, stderr = TRUE, stdin = NULL,
                          wait = TRUE, timeout = NULL,
                          verbose = TRUE) {
    # set working directory ---------------------
    if (!is.null(wd)) {
        if (!dir.exists(wd) &&
            !dir.create(wd, showWarnings = FALSE)) {
            cli::cli_abort(
                "Cannot create working directory {.path {wd}"
            )
        }
        if (verbose) {
            cli::cli_inform("Setting working directory: {.path {wd}")
        }
        old_wd <- getwd()
        setwd(wd)
        on.exit(setwd(old_wd), add = TRUE)
    }
    if (verbose) {
        cli::cli_inform(paste(
            "Running command {.field {command}}",
            "{.field {paste(command_params, collapse = ' ')}}"
        ))
        cli::cat_line()
    }
    system2(
        command = command,
        args = command_params,
        stdout = stdout %||% "",
        stderr = stderr %||% "",
        stdin = stdin %||% "",
        wait = wait,
        timeout = timeout %||% 0L
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
    if (is.null(old)) {
        old <- Sys.getenv(name, unset = NA_character_, names = FALSE)
    }
    if (!is.na(new) && !is.na(old)) {
        if (action == "prefix") {
            new <- paste(new, old, sep = sep)
        } else if (action == "suffix") {
            new <- paste(old, new, sep = sep)
        }
    }
    new
}

# For `stdout` and `stderr`
assert_io <- function(x, arg = rlang::caller_arg(x),
                      call = rlang::caller_call()) {
    if (is.null(x)) {

    } else if (rlang::is_string(x)) {
        if (!nzchar(x)) {
            cli::cli_abort(
                "{.arg {arg}} cannot be an empty string",
                call = call
            )
        }
    } else if (!rlang::is_bool(x)) {
        cli::cli_abort(
            "{.arg {arg}} must be a bool or a string of file path",
            call = call
        )
    }
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
