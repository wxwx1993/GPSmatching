test_that("estimate_pmetric_erf works as expected", {


  # Weighting
  set.seed(6451)
  gnm_model <-  estimate_pmetric_erf(Y ~ w + cf5,
                            family = gaussian,
                            data = pseudo_pop_weight_test,
                            ci_appr = "weighting")
  expect_equal(gnm_model$coefficients[3][[1]], 1.341568, tolerance = 0.00001)



  # Matching
  set.seed(3784)
  gnm_model <-  estimate_pmetric_erf(Y ~ w + cf5,
                                     family = gaussian,
                                     data = pseudo_pop_weight_test,
                                     ci_appr = "matching")
  expect_equal(gnm_model$coefficients[3][[1]], 2.789947, tolerance = 0.00001)


  # Wrong ci_appr.
  expect_error(estimate_pmetric_erf(Y ~ s(w) + cf5,
                                    family = gaussian,
                                    data = pseudo_pop_weight_test,
                                    ci_appr = "grounding"))

})
