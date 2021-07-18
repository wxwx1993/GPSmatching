test_that("Set logger works as expected!", {

    logger::log_threshold("INFO")
    set_logger(logger_level = "DEBUG")
    expect_equal(logger::log_threshold()[1], 500L)
    expect_error(update_logger(logger_level = "ABC"))
})
