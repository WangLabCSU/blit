testthat::test_that("`bwa()` works as expected", {
  testthat::skip_if_not(nzchar(Sys.which("bwa")))  # 确保系统安装了BWA
  bwa() |> cmd_help()

  bwa("index") |> cmd_help()
  bwa("mem") |> cmd_help()
})
