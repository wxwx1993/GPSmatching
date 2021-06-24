test_that("absoulte_weighted_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.

  # temp solution.
  # TODO: regenrate pseudo_pop_weight_test to include counter and row_index.
  mydata <- data.frame(pseudo_pop_weight_test[,c("Y","w","gps","gps","gps",
                                                 "ipw","cf1","cf2","cf3",
                                                 "cf4","cf5","cf6",
                                                 "year","region")])

  setDT(mydata)
  val <- absolute_weighted_corr_fun(mydata[,2],
            mydata[,6],
            mydata[,7:length(mydata)]
            )$mean_absolute_corr
  expect_equal(val, 0.110354332937308, tolerance=0.0001)
})
