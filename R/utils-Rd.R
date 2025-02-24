rd_format_code <- function(x) sprintf("`%s`", x)

rd_cmd <- function(cmd) {
    sprintf("A string of path to `%s` command", cmd)
}

rd_dots <- function(cmd, details = TRUE) {
    doc <- sprintf(
        paste(
            "<[dynamic dots][rlang::dyn-dots]>",
            "Additional arguments passed to `%s` command.",
            "Empty arguments are automatically trimmed.",
            "If a single argument, such as a file path, contains spaces,",
            "it must be quoted, for example using [`shQuote`]"
        ), 
        cmd
    )
    if (details) {
        doc <- sprintf("%s. Details see: `cmd_help(%s())`", doc, cmd)
    }
    doc
}
