test_that("absoulte_weighted_corr_fun works as expected.", {

  # see test-check_covar_balance.R for more details about the test data.
  val <- absolute_weighted_corr_fun(pseudo_pop_weight_test[,2],
            pseudo_pop_weight_test[,4],
            pseudo_pop_weight_test[,5:length(pseudo_pop_weight_test)]
            )$mean_absolute_corr
  expect_equal(val, 0.110354332937308, tolerance=0.0001)
})
