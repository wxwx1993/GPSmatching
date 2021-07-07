test_that("estimate-semi-erf works as expected ", {

  set.seed(245)
  gam_model <-  estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                  family = gaussian,
                                  data = pseudo_pop_weight_test,
                                  ci_appr = "weighting")
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 6.924682, tolerance = 0.00001)

})
