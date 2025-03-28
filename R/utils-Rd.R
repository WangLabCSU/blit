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
            "it must be quoted, for example using [`shQuote()`]"
        ),
        cmd
    )
    if (details) {
        doc <- sprintf("%s. Details see: %s", doc, rd_help(cmd))
    }
    doc
}

rd_help <- function(cmd) sprintf("`cmd_help(%s())`", cmd)

rd_seealso <- function() {
    paste(
        "- [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]/[`cmd_conda()`]",
        "- [`cmd_on_start()`]/[`cmd_on_exit()`]",
        "- [`cmd_on_succeed()`]/[`cmd_on_fail()`]",
        "- [`cmd_parallel()`]",
        sep = "\n"
    )
}
