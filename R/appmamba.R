#' Manage Environment with `micromamba`
#'
#' @describeIn appmamba `blit` utilizes `micromamba` to manage environments.
#' This function simply executes the specified `micromamba` command.
#'
#' @param ... <[dynamic dots][rlang::dyn-dots]> Additional arguments passed to
#'   `micromamba`. Run `appmamba()` for more details.
#' @examples
#' install_appmamba()
#' appmamba()
#' appmamba("env", "list")
#' # uninstall_appmamba() # Uninstall the `micromamba`
#' @export
appmamba <- function(...) {
    path <- app_dir("appmamba")
    if (!dir.exists(path)) {
        if (interactive()) {
            cli::cli_inform("Would you like to install {.pkg appmamba}?")
            if (utils::menu(c("Yes", "No")) == 1L) {
                install_appmamba()
            }
        } else {
            cli::cli_abort(c(
                "{.pkg appmamba} is not installed",
                i = "Please install it with {.fn install_appmamba}"
            ))
        }
    }
    command <- file.path(path, "bin", "micromamba")
    if (!file.exists(command)) {
        cli::cli_abort(c(
            "Invalid installation of {.pkg appmamba}",
            i = paste(
                "Please reinstall {.pkg appmamba} with",
                "{.code install_appmamba(force = TRUE)}"
            )
        ))
    }
    args <- rlang::dots_list(..., .ignore_empty = "all")
    args <- as.character(unlist(args, FALSE, FALSE))
    if (length(args) == 0L) args <- "--help"
    args <- c("--root-prefix", appmamba_root(), args)
    command <- exec(command, args)
    proc <- processx_command(
        command,
        help = FALSE,
        stdout = TRUE,
        stderr = TRUE,
        stdin = NULL,
        verbose = FALSE,
        echo_command = TRUE
    )
    proc$.blit_run()
    invisible(proc$get_exit_status())
}

#' @describeIn appmamba Install appmamba (`micromamba`).
#'
#' @param force A logical value indicating whether to reinstall `appmamba`
#'   if it is already installed.
#' @export
install_appmamba <- function(force = FALSE) {
    cli::cli_inform("Installing {.pkg appmamba}")
    path <- app_dir("appmamba")
    if (dir.exists(path)) {
        if (isTRUE(force)) {
            cli::cli_inform("Removing previous installed {.pkg appmamba}")
            status <- unlink(path, recursive = TRUE, force = TRUE)
            if (status != 0L) {
                cli::cli_abort(
                    "Cannot uninstall {.pkg appmamba} [status = {status}]"
                )
            }
        } else {
            cli::cli_inform("{.pkg appmamba} is already installed")
            return(invisible(TRUE))
        }
    }
    dir_create(path, recursive = TRUE)
    installer <- appmamba_installer_download(appmamba_installer_url())
    status <- utils::untar(installer, exdir = path)
    if (status != 0L) {
        cli::cli_abort(
            "Cannot decompress {.path installer} [status = {status}]"
        )
    }
    cli::cli_inform("Install {.pkg appmamba} successfully!")
    invisible()
}

#' @describeIn appmamba Remove appmamba (`micromamba`).
#' @export
uninstall_appmamba <- function() {
    path <- app_dir("appmamba")
    if (dir.exists(path)) {
        status <- unlink(path, recursive = TRUE, force = TRUE)
        if (status != 0L) {
            cli::cli_abort(
                "Cannot uninstall {.pkg appmamba} [status = {status}]"
            )
        }
        cli::cli_inform("{.pkg appmamba} is uninstalled")
    } else {
        cli::cli_inform("{.pkg appmamba} is not installed")
    }
    invisible()
}

#' @describeIn appmamba Get the `run commands` config file of the `micromamba`.
#' @param edit A logical value indicating whether to open the file for editing.
#' @export
appmamba_rc <- function(edit = FALSE) {
    if (!appmamba_installed()) {
        cli::cli_abort(c(
            "{.pkg appmamba} is not installed",
            i = "Please install it with {.fn install_appmamba}"
        ))
    }
    rc_file <- file.path(appmamba_root(), ".mambarc")
    if (isTRUE(edit)) {
        if (!file.exists(rc_file)) {
            write_lines(c(
                "# For more information about this file see:",
                "# https://conda.io/docs/user-guide/configuration/use-condarc.html"
            ), rc_file)
        }
        utils::file.edit(rc_file, title = "appmamba", fileEncoding = "UTF-8")
        return(invisible(rc_file))
    }
    rc_file
}

appmamba_installed <- function() dir.exists(app_dir("appmamba"))

appmamba_root <- function() {
    path <- file.path(data_dir(), "appmamba")
    dir_create(path.expand(path))
}

appmamba_use_system <- function() {
    system <- Sys.getenv("BLIT_APPMAMBA_SYSTEM", NA_character_)
    !is.na(system) && (
        identical(system, "1") ||
            identical(system, "TRUE") ||
            identical(system, "True") ||
            identical(system, "true")
    )
}

appmamba_installer_download <- function(url) {
    # reuse an already-existing installer
    installer <- file.path(cache_dir(), "appmamba")
    download_file(url, installer, method = "libcurl")
}

appmamba_installer_url <- function() {
    url <- getOption("blit.appmamba.url")
    if (!is.null(url)) {
        return(url)
    }
    info <- Sys.info()
    base <- "https://micro.mamba.pm/api/micromamba"
    if (is_windows()) {
        name <- "win-64"
    } else if (is_linux()) {
        if (info["machine"] == "x86_64") {
            name <- "linux-64"
        } else if (info["machine"] == "arm64") {
            name <- "linux-aarch64"
        } else {
            name <- "linux-ppc64le"
        }
    } else if (is_osx()) {
        if (info["machine"] == "x86_64") {
            name <- "osx-64"
        } else {
            name <- "osx-arm64"
        }
    } else {
        cli::cli_abort(
            "unsupported platform {.field {Sys.info()[\"sysname\"]}}"
        )
    }
    file.path(base, name, "latest")
}
