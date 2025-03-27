#' Run alleleCount
#'
#' The `alleleCount` program primarily exists to prevent code duplication
#' between some other projects, specifically `AscatNGS` and `Battenberg`.
#'
#' @param hts_file A string of path to sample HTS file.
#' @param loci_file A string of path to loci file.
#' @param ofile A string of path to the output file.
#' @param odir A string of path to the output directory.
#' @param ... `r rd_dots("alleleCounter")`.
#' @param alleleCounter `r rd_cmd("alleleCounter")`.
#' @family command
#' @inherit exec return
#' @seealso
#' - <https://github.com/cancerit/alleleCount>
#'
#' `r rd_seealso()`
#' @export
allele_counter <- make_command(
    "allele_counter",
    function(
        hts_file,
        loci_file,
        ofile,
        ...,
        odir = getwd(),
        alleleCounter = NULL
    ) {
        assert_string(alleleCounter, allow_empty = FALSE, allow_null = TRUE)
        AlleleCounter$new(,
            cmd = alleleCounter,
            ...,
            hts_file = hts_file,
            loci_file = loci_file,
            ofile = ofile,
            odir = odir
        )
    }
)

AlleleCounter <- R6Class(
    "AlleleCounter",
    inherit = Command,
    private = list(
        alias = function() "alleleCounter",
        setup_help_params = function() "--help",
        setup_command_params = function(hts_file, loci_file, ofile, odir) {
            opath <- build_opath(odir, ofile)
            c(
                arg0("-l", loci_file),
                arg0("-b", hts_file),
                arg0("-o", opath)
            )
        }
    )
)
