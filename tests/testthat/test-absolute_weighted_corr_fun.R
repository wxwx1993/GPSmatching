test_that("absoulte_weighted_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.

  # temp solution.
  # TODO: regenrate pseudo_pop_weight_test to include counter and row_index.
  data <- data.frame(pseudo_pop_weight_test[,c("Y","w","gps","gps","gps",
                                               "ipw","cf1","cf2","cf3",
                                               "cf4","cf5","cf6",
                                               "year","region")])

  data1 <- as.data.table(data)
  val1 <- absolute_weighted_corr_fun(data1[,2],
            data1[,6],
            data1[,7:length(data1)]
            )
  expect_equal(val1$mean_absolute_corr, 0.1103543, tolerance=0.0001)
  expect_equal(val1$median_absolute_corr, 0.106839, tolerance=0.0001)
  expect_equal(val1$maximal_absolute_corr, 0.1668302, tolerance = 0.0001)

  # use data.frame instead of data.table (w)
  expect_error(absolute_weighted_corr_fun(data[,2],
                                          data[,6],
                                          data[,7:length(data)]
  ))


  # Use data that cause missing value in the results.
  data2 <- data
  data2$region <- "East"
  data2$region <- as.factor(data2$region)
  setDT(data2)
  expect_warning(absolute_weighted_corr_fun(data2[,2],
                                            data2[,6],
                                            data2[,7:length(data2)]
  ))

})
