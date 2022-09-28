test_that("absoulte_weighted_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.

  data1 <- setDT(pseudo_pop_weight_test)
  val1 <- absolute_weighted_corr_fun(data1[,2],
            data1[,13],
            data1[,5:12]
            )
  expect_equal(val1$mean_absolute_corr, 0.09003267, tolerance=0.0001)
  expect_equal(val1$median_absolute_corr, 0.07821234, tolerance=0.0001)
  expect_equal(val1$maximal_absolute_corr, 0.2222087, tolerance = 0.0001)


  # Use data that cause missing value in the results.
  data2 <- data1
  data2$region <- "East"
  data2$region <- as.factor(data2$region)
  expect_warning(absolute_weighted_corr_fun(data2[,2],
                                            data2[,6],
                                            data2[,7:length(data2)]
  ))

  # use data.frame instead of data.table (w)
  data3 <- setDF(data1)
  expect_error(absolute_weighted_corr_fun(data3[,2],
                                          data3[,6],
                                          data3[,7:length(data3)]
  ))

})
