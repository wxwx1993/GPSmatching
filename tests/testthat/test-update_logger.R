test_that("Update logger works as expected!", {

    logger::log_threshold("INFO")
    update_logger(logger_level = "DEBUG")
    expect_equal(logger::log_threshold()[1], 500L)
    expect_error(update_logger(logger_level = "ABC"))
})
