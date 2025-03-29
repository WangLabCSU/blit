testthat::test_that("`BlitProcess` stdin works well", {
    proc <- BlitProcess$new("echo", "blit is awesome", stdin = FALSE)
    on.exit(proc$.blit_kill(), add = TRUE)
    testthat::expect_identical(proc$get_input_file(), NULL)
    proc2 <- BlitProcess$new("echo", "blit is awesome", stdin = TRUE)
    on.exit(proc2$.blit_kill(), add = TRUE)
    if (processx::is_valid_fd(0L)) {
        testthat::expect_identical(proc2$get_input_file(), "")
    } else {
        testthat::expect_identical(proc2$get_input_file(), NULL)
    }
})

testthat::test_that("`BlitProcess` stdout works well", {
    # FALSE
    proc <- BlitProcess$new("echo", "blit is awesome", stdout = FALSE)
    on.exit(proc$.blit_kill(), add = TRUE)
    testthat::expect_identical(proc$get_output_file(), NULL)

    # TRUE
    proc2 <- BlitProcess$new("echo", "blit is awesome", stdout = TRUE)
    on.exit(proc2$.blit_kill(), add = TRUE)
    if (processx::is_valid_fd(1L)) {
        testthat::expect_identical(proc2$get_output_file(), "")
    } else {
        testthat::expect_identical(proc2$get_output_file(), NULL)
    }

    # `TRUE` and `callback`
    proc3 <- BlitProcess$new(
        "echo", "blit is awesome",
        stdout = TRUE,
        .blit_stdout_callback = function(lines, proc) lines
    )
    on.exit(proc3$.blit_kill(), add = TRUE)
    testthat::expect_identical(proc3$get_output_file(), "|")

    # `file` and `callback`
    tmp <- tempfile()
    proc4 <- BlitProcess$new(
        "echo", "blit is awesome",
        stdout = tmp,
        .blit_stdout_callback = function(lines, proc) lines
    )
    on.exit(proc4$.blit_kill(), add = TRUE)
    on.exit(file.remove(tmp), add = TRUE)
    testthat::expect_identical(proc4$get_output_file(), "|")
    proc4$.blit_run()
    testthat::expect_identical(readLines(tmp), "blit is awesome")
})

testthat::test_that("`BlitProcess` stderr works well", {
    # FALSE
    proc <- BlitProcess$new("echo", "blit is awesome", stderr = FALSE)
    on.exit(proc$.blit_kill(), add = TRUE)
    testthat::expect_identical(proc$get_error_file(), NULL)

    # TRUE
    proc2 <- BlitProcess$new("echo", "blit is awesome", stderr = TRUE)
    on.exit(proc2$.blit_kill(), add = TRUE)
    if (processx::is_valid_fd(2L)) {
        testthat::expect_identical(proc2$get_error_file(), "")
    } else {
        testthat::expect_identical(proc2$get_error_file(), NULL)
    }

    # `TRUE` and `callback`
    proc3 <- BlitProcess$new(
        "echo", "blit is awesome",
        stderr = TRUE,
        .blit_stderr_callback = function(lines, proc) lines
    )
    on.exit(proc3$.blit_kill(), add = TRUE)
    testthat::expect_identical(proc3$get_error_file(), "|")

    # `file` and `callback`
    tmp <- tempfile()
    proc4 <- BlitProcess$new(
        "echo", "blit is awesome",
        stderr = tmp,
        .blit_stderr_callback = function(lines, proc) lines
    )
    on.exit(proc4$.blit_kill(), add = TRUE)
    on.exit(file.remove(tmp), add = TRUE)
    testthat::expect_identical(proc4$get_error_file(), "|")
    proc4$.blit_run()
    testthat::expect_identical(readLines(tmp), character())
})
