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

#' @importFrom utils getFromNamespace
rd_collect_family <- function(family,
                              section_title = paste(family, "family"),
                              code_style = TRUE) {
    # get blocks objects from the roxygenize function
    blocks <- NULL
    pos <- sys.nframe()
    while (pos > 0L) {
        if (!is.null(call <- sys.call(-pos))) {
            fn <- eval(.subset2(call, 1L), sys.frame(-(pos + 1L)))
            env <- sys.frame(-pos)
            if (identical(fn, getFromNamespace("roxygenize", "roxygen2")) &&
                exists("blocks", envir = env, inherits = FALSE)) {
                blocks <- get("blocks", envir = env, inherits = FALSE)
                break
            }
        }
        pos <- pos - 1L
    }
    blocks <- blocks[
        vapply(blocks, function(block) {
            getFromNamespace("block_has_tags", "roxygen2")(block, "family") &&
                identical(
                    getFromNamespace("block_get_tag_value", "roxygen2")(
                        block, "family"
                    ),
                    family
                )
        }, logical(1L), USE.NAMES = FALSE)
    ]
    if (length(blocks) == 0L) {
        return(character())
    }
    funs <- vapply(blocks, function(block) {
        as.character(.subset2(block$call, 2L))
    }, character(1L), USE.NAMES = FALSE)
    if (code_style) {
        items <- sprintf("\\code{\\link[=%s]{%s()}}", funs, funs)
    } else {
        items <- sprintf("\\link[=%s]{%s()}", funs, funs)
    }
    c(
        sprintf("@section %s:", section_title),
        "\\itemize{",
        sprintf("  \\item %s", items),
        "}"
    )
}

rd_seealso <- function() {
    paste(
        "- [`cmd_wd()`]/[`cmd_envvar()`]/[`cmd_envpath()`]",
        "- [`cmd_run()`]/[`cmd_help()`]",
        sep = "\n"
    )
}
