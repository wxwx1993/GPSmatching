test_that("estimate-semi-erf works as expected ", {

  # Weighting
  set.seed(245)
  gam_model <-  estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                  family = gaussian,
                                  data = pseudo_pop_weight_test,
                                  ci_appr = "weighting")
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 5.484447, tolerance = 0.00001)


  # Matching
  set.seed(897)
  gam_model <-  estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                         family = gaussian,
                                         data = pseudo_pop_weight_test,
                                         ci_appr = "matching")
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 7.350412, tolerance = 0.00001)



  # Wrong ci_appr.
  expect_error(estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                        family = gaussian,
                                        data = pseudo_pop_weight_test,
                                        ci_appr = "grounding"))


})
