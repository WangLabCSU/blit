#' Run GISTIC2
#'
#' @description The GISTIC module identifies regions of the genome that are
#' significantly amplified or deleted across a set of samples. Each aberration
#' is assigned a G-score that considers the amplitude of the aberration as well
#' as the frequency of its occurrence across samples. False Discovery Rate
#' q-values are then calculated for the aberrant regions, and regions with
#' q-values below a user-defined threshold are considered significant. For each
#' significant region, a "peak region" is identified, which is the part of the
#' aberrant region with greatest amplitude and frequency of alteration. In
#' addition, a "wide peak" is determined using a leave-one-out algorithm to
#' allow for errors in the boundaries in a single sample. The "wide peak"
#' boundaries are more robust for identifying the most likely gene targets in
#' the region. Each significantly aberrant region is also tested to determine
#' whether it results primarily from broad events (longer than half a chromosome
#' arm), focal events, or significant levels of both. The GISTIC module reports
#' the genomic locations and calculated q-values for the aberrant regions. It
#' identifies the samples that exhibit each significant amplification or
#' deletion, and it lists genes found in each "wide peak" region.
#'
#' @param seg A data.frame of segmented data.
#' @param refgene Path to reference genome data input file (REQUIRED, see below
#' for file description).
#' @param ... `r rd_dots("gistic2")`.
#' @inheritParams allele_counter
#' @param gistic2 `r rd_cmd("gistic2")`.
#' @seealso <https://broadinstitute.github.io/gistic2/>
#' @inherit exec return seealso
#' @family command
#' @export
gistic2 <- make_command(
    "gistic2",
    function(seg, refgene, ...,
             odir = getwd(), gistic2 = NULL) {
        assert_string(gistic2, allow_empty = FALSE, allow_null = TRUE)
        Gistic2$new(
            cmd = gistic2, ..., odir = odir,
            seg = seg, refgene = refgene
        )
    }
)

Gistic2 <- R6Class(
    "Gistic2",
    inherit = Command,
    private = list(
        name = "gistic2",
        setup_command_params = function(seg, refgene, odir) {
            assert_s3_class(seg, "data.frame", "a data frame")
            odir <- build_opath(odir)
            seg_file <- tempfile("gistic2")
            data.table::fwrite(seg, file = seg_file, sep = "\t")
            private$setup_exit(file.remove(seg_file))
            c(
                arg_internal("-seg", seg_file),
                arg_internal("-refgene", refgene),
                arg_internal("-b", odir)
            )
        }
    )
)
