test_that("absoulte_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.
  val <- absolute_corr_fun(pseudo_pop_covar_test[,2],
         pseudo_pop_covar_test[,4:length(pseudo_pop_covar_test)])
  val <- val$mean_absolute_corr
  expect_equal(val, 0.2826464, tolerance=0.0001)

})
