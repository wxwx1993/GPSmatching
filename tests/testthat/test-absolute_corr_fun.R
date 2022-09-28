test_that("absoulte_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.
  val <- absolute_corr_fun(pseudo_pop_covar_test[,2],
         pseudo_pop_covar_test[,4:length(pseudo_pop_covar_test)])

  expect_equal(val$mean_absolute_corr, 0.2826464, tolerance=0.0001)
  expect_equal(val$median_absolute_corr, 0.2850419, tolerance=0.0001)
  expect_equal(val$maximal_absolute_corr, 0.4719418, tolerance=0.0001)
  expect_equal(length(val$absolute_corr), 6)

  # use data.frame instead of data.table (w)
  w1 <- as.data.frame(pseudo_pop_covar_test[,2])
  expect_error(absolute_corr_fun(w1,
                 pseudo_pop_covar_test[,4:length(pseudo_pop_covar_test)]))

  # use data.frame instead of data.table (c)
  c1 <- as.data.frame(pseudo_pop_covar_test[,4:length(pseudo_pop_covar_test)])
  expect_error(absolute_corr_fun(pseudo_pop_covar_test[,2],
                                 c1))


  # Test with categorical data
  data2 <- as.data.table(pseudo_pop_weight_test)
  val2 <- absolute_corr_fun(data2[,2],
                            data2[,5:12])

  expect_equal(val2$mean_absolute_corr, 0.1514455, tolerance=0.0001)
  expect_equal(val2$median_absolute_corr, 0.1208244, tolerance=0.0001)
  expect_equal(val2$maximal_absolute_corr, 0.3078682, tolerance=0.0001)
  expect_equal(length(val2$absolute_corr), 8)

  # Test with categorical data while generating missing values
  data3 <- as.data.table(pseudo_pop_weight_test)
  data3$region <- "East"
  expect_warning(absolute_corr_fun(data3[,2],
                            data3[,5:length(data3)]))

})
