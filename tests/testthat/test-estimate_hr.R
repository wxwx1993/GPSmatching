test_that("estimate_hr works as expected", {

  set.seed(6451)
  gnm_model <-  estimate_hr(Y ~ s(w) + cf5,
                            family = gaussian,
                            data = pseudo_pop_covar_test)
  expect_equal(gnm_model$coefficients[3][[1]], 2.032414, tolerance = 0.00001)
})
