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
        if (!Sys.chmod(file, "555")) {
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
    if (unlink(x = path, force = TRUE)) {
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
    cli::cli_inform("Downloading from {.path {url}}")
    if (utils::download.file(url = url, destfile = path, ...) > 0L) {
        cli::cli_abort("Cannot download {.path {url}}")
    }
}

read_lines <- function(path, n = Inf) {
    # data.table don't support xz compression
    data.table::fread(
        file = path, sep = "", header = FALSE,
        colClasses = "character",
        showProgress = FALSE,
        nrows = n
    )[[1L]]
}

# To write a file with windows line endings use write_lines(eol = "\r\n")
write_lines <- function(text, path, append = FALSE,
                        eol = if (.Platform$OS.type == "windows") {
                            "\r\n"
                        } else {
                            "\n"
                        },
                        compress = "auto") {
    data.table::fwrite(
        x = list(text),
        file = path,
        append = append,
        quote = FALSE,
        eol = eol,
        na = "NA",
        col.names = FALSE,
        logical01 = FALSE,
        showProgress = FALSE,
        verbose = FALSE,
        compress = compress
    )
    invisible(text)
}

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
