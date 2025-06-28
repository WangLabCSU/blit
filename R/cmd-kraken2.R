#' Running Kraken2
#'
#' Kraken is a taxonomic sequence classifier that assigns taxonomic labels to
#' DNA sequences. Kraken examines the k-mers within a query sequence and uses
#' the information within those k-mers to query a database. That database maps
#' k-mers to the lowest common ancestor (LCA) of all genomes known to contain a
#' given k-mer.
#'
#' @param reads A character vector of FASTQ files used as input to Kraken2.
#' Can be one file (single-end) or two files (paired-end).
#' @param ... `r rd_dots("kraken2")`.
#' @param ofile A string of path to save kraken2 output.
#' @param report A string of path to save kraken2 report.
#' @param classified_out A string of path to save classified sequences, which
#' should be a fastq file.
#' @param unclassified_out A string of path to save unclassified sequences,
#' which should be a fastq file.
#' @inheritParams allele_counter
#' @param kraken2 `r rd_cmd("kraken2")`.
#' @seealso
#'  - <https://github.com/DerrickWood/kraken2/wiki/Manual>
#'  - <https://benlangmead.github.io/aws-indexes/k2>
#'
#' `r rd_seealso()`
#' @family command
#' @inherit exec return
#' @export
kraken2 <- make_command(
    "kraken2",
    function(
        reads, ...,
        ofile = "kraken_output.txt",
        report = "kraken_report.txt",
        classified_out = NULL,
        unclassified_out = NULL,
        odir = getwd(),
        kraken2 = NULL
    ) {
        assert_string(kraken2, allow_empty = FALSE, allow_null = TRUE)
        Kraken2$new(
            cmd = kraken2,
            ...,
            reads = reads,
            ofile = ofile,
            report = report,
            classified_out = classified_out,
            unclassified_out = unclassified_out,
            odir = odir,
        )
    }
)

Kraken2 <- R6Class(
    "Kraken2",
    inherit = Command,
    private = list(
        alias = function() "kraken2",
        setup_help_params = function() "--help",
        setup_command_params = function(
            reads,
            ofile,
            report,
            classified_out,
            unclassified_out,
            odir
        ) {
            reads <- as.character(reads)
            if (length(reads) < 1L || length(reads) > 2L) {
                cli::cli_abort("{.arg reads} must be of length 1 or 2")
            }
            assert_string(ofile, allow_null = TRUE)
            assert_string(report, allow_null = TRUE)
            assert_string(classified_out, allow_null = TRUE)
            # https://github.com/DerrickWood/kraken2/wiki/Manual
            # Usage of --paired also affects the --classified-out and
            # --unclassified-out options; users should provide a # character in
            # the filenames provided to those options, which will be replaced by
            # kraken2 with "_1" and "_2" with mates spread across the two files
            # appropriately. For example:
            odir <- build_opath(odir)
            if (!is.null(classified_out)) {
                if (!is.null(fq2)) {
                    classified_out <- sprintf("%s#", classified_out)
                }
                classified_out <- file_path(odir, classified_out)
            }
            if (!is.null(unclassified_out)) {
                if (!is.null(fq2)) {
                    unclassified_out <- sprintf("%s#", unclassified_out)
                }
                unclassified_out <- file_path(odir, unclassified_out)
            }
            if (!is.null(ofile)) ofile <- file_path(odir, ofile)
            if (!is.null(report)) report <- file_path(odir, report)
            opath <- c(ofile, report)
            c(
                arg0(
                    "--classified-out",
                    classified_out,
                    allow_null = TRUE
                ),
                arg0(
                    "--unclassified-out",
                    unclassified_out,
                    allow_null = TRUE
                ),
                arg0("--output", ofile, allow_null = TRUE),
                arg0("--report", report, allow_null = TRUE),
                if (length(reads) == 2L) "--paired",
                reads
            )
        }
    )
)
