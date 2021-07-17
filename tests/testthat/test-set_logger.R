test_that("Set logger works as expected!", {

    logger::log_threshold("INFO")
    set_logger(logger_level = "DEBUG")
    expect_equal(logger::log_threshold()[1], 500L)
    expect_error(update_logger(logger_level = "ABC"))

    set_logger("mylogger.log", "TRACE")
    tmp <- get_logger()
    expect_equal(tmp$logger_file_path, "mylogger.log")
    expect_equal(tmp$logger_level, "TRACE")

})
