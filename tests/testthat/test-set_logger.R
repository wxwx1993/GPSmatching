test_that("Set logger works as expected!", {

    skip_on_cran()
    logger::log_threshold("INFO")
    set_logger(logger_level = "DEBUG")
    expect_equal(logger::log_threshold()[1], 500L)
    expect_error(update_logger(logger_level = "ABC"))
    expect_error(set_logger(logger_level = "ABC"))
    expect_error(set_logger(logger_level = NULL))

    lg <- get_logger()
    expect_equal(lg$logger_file_path, "CausalGPS.log")
    expect_equal(lg$logger_level, "DEBUG")
})
