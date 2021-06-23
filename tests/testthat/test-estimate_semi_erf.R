test_that("estimate-semi-erf works as expected ", {

  set.seed(245)
  gam_model <-  estimate_semi_erf(Y ~ s(w) + cf5,
                                  family = gaussian,
                                  data = pseudo_pop_covar_test)
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 5.40743052522851, tolerance = 0.00001)

})
