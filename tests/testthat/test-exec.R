testthat::test_that("`allele_counter()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("alleleCounter")))
    allele_counter() |> cmd_help()
})

testthat::test_that("`bwa()` works as expected", {
  testthat::skip_if_not(nzchar(Sys.which("bwa")))
  bwa() |> cmd_help()
  bwa("index") |> cmd_help()
  bwa("mem") |> cmd_help()
})

testthat::test_that("`cellranger()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("cellranger")))
    cellranger() |> cmd_help()
})

testthat::test_that("`fastq_pair()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("fastq_pair")))
    fastq_pair() |> cmd_help()
})

testthat::test_that("`gistic2()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("gistic2")))
    gistic2() |> cmd_help()
})

testthat::test_that("`kraken_tools()` works as expected", {
    testthat::skip_if_not(
        nzchar(Sys.which("python2")) || nzchar(Sys.which("python3"))
    )
    for (script in KrakenToolsScripts) {
        kraken_tools(script) |> cmd_help()
    }
})

testthat::test_that("`kraken2()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("kraken2")))
    kraken2() |> cmd_help()
})

testthat::test_that("`perl()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("perl")))
    perl() |> cmd_help()
})

testthat::test_that("`python()` works as expected", {
    testthat::skip_if_not(
        nzchar(Sys.which("python2")) || nzchar(Sys.which("python3"))
    )
    python() |> cmd_help()
})

testthat::test_that("`seqkit()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("seqkit")))
    seqkit() |> cmd_help()
})

testthat::test_that("`trust4()` works as expected", {
    testthat::skip_if_not(nzchar(Sys.which("run-trust4")))
    trust4() |> cmd_help()
})

testthat::test_that("`pipe()` method works well", {
    tmpdir <- tempdir()
    file <- tempfile(tmpdir = tmpdir)
    write_lines(letters, path = file)
    on.exit(file.remove(file))
    file2 <- tempfile()
    on.exit(file.remove(file2), add = TRUE)
    exec("gzip", "-c", file) |>
        exec("gzip", "-d", ">", file2) |>
        cmd_run()
    testthat::expect_identical(read_lines(file), read_lines(file2))
})
