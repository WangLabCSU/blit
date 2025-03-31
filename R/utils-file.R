blit_dir <- function(which) {
    dir_create(tools::R_user_dir(pkg_nm(), which), recursive = TRUE)
}

data_dir <- function() blit_dir("data")

cache_dir <- function() blit_dir("cache")

config_dir <- function() blit_dir("config")

path_ext_remove <- function(path) {
    sub("\\.[[:alnum:]]*$", "", path, perl = TRUE)
}

path_ext_set <- function(path, ext) {
    if (!is.null(ext) && nzchar(ext)) {
        path <- paste(path_ext_remove(path), ext, sep = ".")
    }
    path
}

path_ext <- function(path) {
    matches <- regexpr("\\.([[:alnum:]]+)$", path, perl = TRUE)
    start <- as.vector(matches)
    end <- start + attr(matches, "match.length") - 1L
    ifelse(start == -1L, "", substr(path, start + 1L, end))
}

path_trim <- function(path) {
    # remove trailing backslash or slash
    sub("(\\\\+|/+)$", "", path, perl = TRUE)
}

path_equal <- function(path1, path2) {
    normalizePath(path1, "/", FALSE) == normalizePath(path2, "/", FALSE)
}

file_executable <- function(file) {
    if (file.access(file, mode = 1L)) {
        if (!Sys.chmod(file, "755")) {
            cli::cli_abort("{.path {file}} is not executable")
        }
    }
}

file_path <- function(..., ext = NULL) {
    path <- file.path(..., fsep = "/")
    if (!is.null(ext)) path <- paste(path, ext, sep = ".")
    path
}

file_delete <- function(path) {
    if (file.exists(path) && unlink(x = path, force = TRUE)) {
        cli::cli_abort("Canno remove {.path {path}}")
    }
    invisible(path)
}

dir_create <- function(path, ...) {
    if (!dir.exists(path) &&
        !dir.create(path = path, showWarnings = FALSE, ...)) {
        cli::cli_abort("Cannot create directory {.path {path}}")
    }
    invisible(path)
}

dir_delete <- function(path) {
    if (dir.exists(path) && unlink(x = path, recursive = TRUE, force = TRUE)) {
        cli::cli_abort("Canno remove {.path {path}}")
    }
    invisible(path)
}

#' Will always add the basename of file into the exdir
#' @noRd
unzip2 <- function(path, exdir, ..., basename = TRUE, overwrite = TRUE) {
    dir_create(exdir)
    if (basename) {
        exdir <- dir_create(file.path(exdir, base::basename(path)))
    }
    if (is.null(utils::unzip(path, exdir = exdir, ..., overwrite = overwrite))) {
        cli::cli_abort("Cannot unzip {.path {path}}")
    }
    exdir
}

download_file <- function(url, path, ...) {
    if (file.exists(path)) {
        cli::cli_inform("Using cached file: {.path {path}}")
        return(path)
    }
    cli::cli_inform("Downloading from {.path {url}}")
    status <- utils::download.file(url = url, destfile = path, ...)
    if (status > 0L) {
        cli::cli_abort("Cannot download {.path {url}} [status = {status}]")
    }
    invisible(path)
}

read_lines <- function(path, n = Inf) {
    if (is_installed("data.table")) {
        if (n < 0L) n <- Inf
        getExportedValue("data.table", "fread")(
            file = path, sep = "", header = FALSE,
            colClasses = "character",
            showProgress = FALSE,
            nrows = n
        )[[1L]]
    } else if (is_installed("brio")) {
        if (is.infinite(n) || n < 0L) n <- -1L
        out <- getExportedValue("brio", "read_lines")(path, n = n)
        out[out == "NA"] <- NA_character_
        out
    } else if (is_installed("vroom")) {
        if (n < 0L) n <- Inf
        getExportedValue("vroom", "vroom_lines")(path, n_max = n, na = "NA")
    } else if (is_installed("readr")) {
        if (n < 0L) n <- Inf
        getExportedValue("readr", "read_lines")(path, n_max = n, na = "NA")
    } else {
        if (is.infinite(n) || n < 0L) n <- -1L
        out <- readLines(path, n = n, warn = FALSE)
        out[out == "NA"] <- NA_character_
        out
    }
}

# To write a file with windows line endings use write_lines(eol = "\r\n")
write_lines <- function(text, path, eol = if (.Platform$OS.type == "windows") {
                            "\r\n"
                        } else {
                            "\n"
                        }) {
    if (is_installed("data.table")) {
        getExportedValue("data.table", "fwrite")(
            x = list(text),
            file = path,
            append = FALSE,
            quote = FALSE,
            eol = eol,
            na = "NA",
            col.names = FALSE,
            logical01 = FALSE,
            showProgress = FALSE,
            verbose = FALSE,
            compress = "auto"
        )
    } else if (is_installed("brio")) {
        getExportedValue("brio", "write_lines")(text, path, eol = eol)
    } else if (is_installed("vroom")) {
        getExportedValue("vroom", "vroom_write_lines")(text, path, sep = eol)
    } else if (is_installed("readr")) {
        getExportedValue("readr", "write_lines")(text, path, sep = eol)
    } else {
        writeLines(text, path, sep = eol)
    }
    invisible(text)
}

write_table <- function(data, path, sep = "\t") {
    if (is_installed("data.table")) {
        getExportedValue("data.table", "fwrite")(data, file = path, sep = sep)
    } else if (is_installed("vroom")) {
        getExportedValue("vroom", "vroom_write")(data, file = path, delim = sep)
    } else if (is_installed("readr")) {
        getExportedValue("readr", "write_delim")(data, file = path, delim = sep)
    } else {
        utils::write.table(data, file = path, sep = sep, row.names = FALSE)
    }
}

# Can handle compressed files
read_lines2 <- function(path, n = Inf) {
    if (is_gzip_suffix(path) || is_gzip_signature(path)) {
        path <- gzfile(path, open = "r")
        on.exit(close(path))
    } else if (is_bz2_suffix(path) || is_bz2_signature(path)) {
        path <- bzfile(path, open = "r")
        on.exit(close(path))
    } else if (is_xz_suffix(path)) {
        path <- xzfile(path, open = "r")
        on.exit(close(path))
    }
    if (is.infinite(n) || n < 0L) n <- -1L
    readLines(path, n = n)
}

# https://github.com/Rdatatable/data.table/blob/15c127e99f8d6aab599c590d4aec346a850f1334/R/fread.R#L90
is_tar <- function(file) endsWith(file, ".tar")

is_gzip_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".z") || endsWith(file, ".gz")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".tgz") || endsWith(file, ".taz"))) {
        return(TRUE)
    }
    FALSE
}

is_gzip_signature <- function(file, file_signature = NULL) {
    match_file_signature(file, file_signature, as.raw(c(0x1F, 0x8B)))
}

is_bz2_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".bz2") || endsWith(file, ".bz")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".tbz2") || endsWith(file, ".tbz"))) {
        return(TRUE)
    }
    FALSE
}

is_bz2_signature <- function(file, file_signature = NULL) {
    match_file_signature(file, file_signature, as.raw(c(0x42, 0x5A, 0x68)))
}

is_xz_suffix <- function(file, tar = FALSE) {
    if (endsWith(file, ".xz") || endsWith(file, ".lzma")) {
        return(TRUE)
    } else if (tar && (endsWith(file, ".txz") || endsWith(file, ".tlz"))) {
        return(TRUE)
    }
    FALSE
}

is_zip <- function(file, file_signature = NULL) {
    endsWith(file, ".zip") ||
        match_file_signature(file, file_signature, charToRaw("PK\x03\x04"))
}

match_file_signature <- function(file, file_signature, match) {
    n <- length(match)
    file_signature <- file_signature %||% readBin(file, raw(), n)
    identical(file_signature[seq_len(n)], match)
}
