blit_data_dir <- function() {
    dir_create(tools::R_user_dir(pkg_nm(), "data"), recursive = TRUE)
}

app_dir <- function(dir = blit_data_dir()) {
    dir_create(file.path(dir, "apps"))
}

bin_dir <- function(dir = blit_data_dir()) {
    dir_create(file.path(dir, "bin"))
}

cache_dir <- function(dir = blit_data_dir()) {
    dir_create(file.path(dir, "cache"))
}

app_manifest <- function(app) {
    pkg_extdata("manifest", paste(app, "yaml", sep = "."), mustWork = TRUE)
}

app_install <- function(app, force = FALSE) {
    appdir <- dir_create(file.path(app_dir(), app))
    manifest <- app_manifest(app)
    if (!nzchar(manifest)) {
        cli::cli_abort("Cannot find manifest file for {.pkg {app}}")
    }
    manifest <- manifest_parse(manifest)
    appfile <- file.path(cache_dir(), basename(manifest$url))
    if (dir.exists(appdir)) {
        if (force) {
            dir_delete(appdir)
        } else {
            cli::cli_inform("{.pkg {app}} already installed")
            return(invisible())
        }
    }
    dir_create(appdir)
    oldwd <- setwd(appdir)
    on.exit(setwd(oldwd), add = TRUE)
    on.exit(manifest_write(manifest, appdir), add = TRUE)
    if (file.exists(appfile)) {
        cli::cli_inform("Using cached file {.path {appfile}}")
    } else {
        download_file(manifest$url, appfile, quiet = TRUE)
    }
    cli::cli_inform("Installing {.pkg {app}} of version {manifest$version}")
    manifest <- manifest_run(manifest, "pre_install")
    manifest <- manifest_run(manifest, "installer")
    if (!is.null(manifest$bin)) app_link_bin(manifest$bin, source = appdir)
    manifest <- manifest_run(manifest, "post_install")
}

app_uninstall <- function(app) {
    appdir <- file.path(app_dir(), app)
    if (!dir.exists(appdir)) {
        cli::cli_warn("{.pkg {app}} is not installed")
        return(invisible())
    }
    manifest <- yaml::read_yaml(
        file.path(appdir, "manifest.yaml"),
        readLines.warn = FALSE, eval.expr = FALSE
    )
    manifest_run(manifest, "uninstall")
}

manifest_run <- function(manifest, script, envir = parent.frame()) {
    rlang::try_fetch(
        {
            if (is.character(manifest[[script]])) {
                manifest[[script]] <- parse(
                    text = manifest[[script]],
                    keep.source = FALSE
                )
            }
            if (!is.expression(manifest[[script]]) &&
                !is.function(manifest[[script]])) {
                manifest[[script]] <- NULL
                return(manifest)
            }
            cli::cli_inform("Running {.field {script}} script")
            if (is.expression(manifest[[script]])) {
                `__BLIT_FUNCTION__` <- eval(manifest[[script]], envir = envir)
            }
            if (is.function(`__BLIT_FUNCTION__`)) {
                manifest[[script]] <- `__BLIT_FUNCTION__`
                `__BLIT_FUNCTION__`()
            }
        },
        error = function(cnd) {
            cli::cli_warn(c(
                paste(
                    "Something went wrong when running",
                    "the {.field {script}} script"
                ),
                i = conditionMessage(cnd)
            ))
        }
    )
    manifest
}

manifest_write <- function(manifest, dir, file = "manifest") {
    yaml::write_yaml(
        manifest,
        file.path(dir, paste(file, "yaml", sep = ".")),
        handlers = list(
            expression = function(x) {
                rlang::new_function(list(),
                    body = rlang::expr({ !!!as.list(x) }) # styler: off
                )
            }
        )
    )
}

manifest_parse <- function(path, call = caller_call()) {
    manifest <- yaml::read_yaml(path, readLines.warn = FALSE, eval.expr = FALSE)
    if (is.null(manifest$version) || is.null(manifest$url)) {
        cli::cli_abort("Invalid manifest file: {.path {path}}", call = call)
    }
    if (!is.null(manifest$bin)) {
        manifest$bin <- manifest_parse_pair(manifest$bin)
    }
    manifest
}

manifest_parse_pair <- function(data) {
    bin <- vapply(data, function(x) {
        if (is.list(x)) x <- unlist(x, use.names = FALSE)
        if (length(x) == 0L) {
            ""
        } else if (length(x) == 1L) {
            x
        } else {
            .subset(x, 1L)
        }
    }, character(1), USE.NAMES = FALSE)
    link <- vapply(
        seq_along(bin), function(i) .subset(names(data), i) %||% "",
        character(1),
        USE.NAMES = FALSE
    )
    keep <- nzchar(bin) &
        !duplicated(paste(bin, link, sep = "-"), fromLast = TRUE)
    if (any(keep)) {
        bin <- bin[keep]
        link <- link[keep]
        out <- rlang::set_names(bin, link)
    } else {
        out <- NULL
    }
    out
}

app_link_bin <- function(bin, source, dir = bin_dir()) {
    for (i in seq_along(bin)) {
        from <- .subset(bin, i)
        path <- file.path(source, from, fsep = "/")
        if (!file.exists(path)) {
            cli::cli_warn("Cannot find link source {.path {from}}, skip it")
            next()
        }
        to <- .subset(names(bin), i)
        if (!nzchar(to)) to <- from
        to <- normalizePath(file.path(dir, to),
            winslash = "/", mustWork = FALSE
        )
        if (file.exists(to)) file_delete(to)
        cli::cli_inform("Linking {.path {from}} to {.path {to}}")
        rlang::try_fetch(
            file.symlink(path, to),
            error = function(cnd) {
                cli::cli_warn(c(
                    "Failed to link {.path {from}} to {.path {to}}",
                    i = conditionMessage(cnd)
                ))
            }
        )
    }
}

app_env_set <- function(env) {

}

app_path_add <- function(env) {

}

uninstall_gistic2 <- function() {
    file_delete(file.path(bin_dir(), "gistic2"))
    dir_delete(app_dir("GISTIC2"))
}
