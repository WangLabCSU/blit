#' TRUST4: immune repertoire reconstruction from bulk and single-cell RNA-seq
#' data
#'
#' @param file1 Path to bam file or fastq file.
#' @param file2 Path to the second paired-end read fastq file, only used for
#' `mode = "fastq"`.
#' @param mode One of "bam" or "fastq". If `NULL`, will be inferred from
#' `file1`.
#' @param ref_coordinate Path to the fasta file coordinate and sequence of
#' V/D/J/C genes.
#' @param ref_annot Path to detailed V/D/J/C gene reference file, such as from
#' IMGT database. (default: not used). (recommended).
#' @param ofile
#'  - `trust4`: Prefix of output files. (default: inferred from file prefix).
#'  - `trust4_imgt_annot`: Output file name.
#'  - `trust4_gene_names`: Output file name.
#' @param ...
#'  - `trust4`: `r rd_dots("run-trust4")`.
#'  - `trust4_imgt_annot`: `r rd_dots("trust4_imgt_annot")`.
#' @inheritParams allele_counter
#' @param trust4 `r rd_cmd("run-trust4")`.
#' @seealso 
#' - <https://github.com/liulab-dfci/TRUST4>
#'
#' `r rd_seealso()`
#' @inherit exec return
#' @family command
#' @export
trust4 <- make_command(
    "trust4",
    function(file1, ref_coordinate, ..., file2 = NULL,
             mode = NULL, ref_annot = NULL,
             ofile = NULL, odir = getwd(),
             trust4 = NULL) {
        assert_string(trust4, allow_empty = FALSE, allow_null = TRUE)
        Trust4$new(
            cmd = trust4,
            ..., file1 = file1, ref_coordinate = ref_coordinate,
            file2 = file2, mode = mode, ref_annot = ref_annot,
            ofile = ofile, odir = odir
        )
    }
)

Trust4 <- R6Class(
    "Trust4",
    inherit = Command,
    private = list(
        name = "run-trust4",
        setup_command_params = function(file1, file2, mode, ref_annot,
                                        ref_coordinate, ofile, odir) {
            assert_string(file1, allow_empty = FALSE)
            if (is.null(mode)) {
                if (grepl("(fastq|fq)(\\.gz)?$", file1, perl = TRUE)) {
                    mode <- "fastq"
                } else if (endsWith(file1, "bam")) {
                    mode <- "bam"
                } else {
                    cli::cli_abort(c(
                        "Cannot infer {.arg mode} from {.arg file1}",
                        i = "Please specify {.arg mode} manually"
                    ))
                }
            } else {
                mode <- match.arg(mode, c("bam", "fastq"))
            }
            if (mode == "bam") {
                if (!is.null(file2)) {
                    cli::cli_abort(
                        "{.arg file2} must be {.code NULL} for {.code mode = \"bam\"}"
                    )
                }
                params <- arg_internal("-b", file1)
            } else {
                if (is.null(file2)) {
                    params <- arg_internal("-u", file1)
                } else {
                    params <- c(
                        arg_internal("-1", file1),
                        arg_internal("-2", file2)
                    )
                }
            }
            odir <- build_opath(odir)
            c(
                params,
                arg_internal("-f", ref_coordinate),
                arg_internal("-ref", ref_annot, allow_null = TRUE),
                arg_internal("-o", ofile, allow_null = TRUE),
                arg_internal("--od", odir)
            )
        }
    )
)

# Normally, the file specified by "--ref" is downloaded from IMGT website, For
# example, for human, you can use command
# `perl BuildImgtAnnot.pl Homo_sapien > IMGT+C.fa`
#' @param species Species to extract IMGT annotation, details see
#' <https://www.imgt.org//download/V-QUEST/IMGT_V-QUEST_reference_directory/>.
#' @inheritParams perl
#' @export
#' @rdname trust4
trust4_imgt_annot <- make_command(
    "trust4_imgt_annot",
    function(species = "Homo_sapien", ...,
             ofile = "IMGT+C.fa", odir = getwd(),
             perl = NULL) {
        assert_string(perl, allow_empty = FALSE, allow_null = TRUE)
        Trust4ImgtAnnot$new(
            cmd = perl, ..., species = species, ofile = ofile, odir = odir
        )
    }
)

Trust4ImgtAnnot <- R6Class(
    "Trust4ImgtAnnot",
    inherit = Perl,
    private = list(
        setup_help_params = function() {
            cli::cli_abort("No help document for <Command: trust4_imgt_annot>")
        },
        setup_command_params = function(species, ofile, odir) {
            assert_string(species, allow_empty = FALSE)
            opath <- build_opath(odir, ofile, abs = TRUE)
            c(shQuote(species), ">", opath)
        },
        combine_params = function() {
            script <- pkg_extdata("TRUST4", "BuildImgtAnnot.pl")
            if (file.access(script, mode = 1L) != 0L) {
                Sys.chmod(script, "555")
            }
            c(script, super$combine_params())
        },
        collect_dots = FALSE
    )
)

#' @param imgt_annot Path of IMGT annotation file, created via
#' `trust4_imgt_annot`.
#' @export
#' @rdname trust4
trust4_gene_names <- function(imgt_annot, ofile = "bcr_tcr_gene_name.txt",
                              odir = getwd()) {
    assert_string(imgt_annot, allow_empty = FALSE)
    opath <- build_opath(odir, ofile)
    lines <- read_lines(imgt_annot)
    gene_lines <- grep("^>", lines, value = TRUE, perl = TRUE)
    genes <- sub("^>", "", gene_lines, perl = TRUE)
    genes <- vapply(strsplit(genes, "*", fixed = TRUE),
        `[[`, character(1L), 1L, USE.NAMES = FALSE 
    )
    write_lines(genes, opath)
}
