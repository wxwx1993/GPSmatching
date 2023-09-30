test_that("estimate-semi-erf works as expected ", {

  # Weighting
  skip_on_cran()
  data.table::setDTthreads(1)
  set.seed(245)
  gam_model <-  estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                  family = gaussian,
                                  data = pseudo_pop_weight_test)
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 5.484447, tolerance = 0.00001)


  # Matching
  set.seed(897)
  data_1 <- pseudo_pop_weight_test
  data_1$counter_weight <- (pseudo_pop_weight_test$counter_weight)*0
  gam_model <-  estimate_semipmetric_erf(Y ~ s(w) + cf5,
                                         family = gaussian,
                                         data = data_1)
  coef_val <- gam_model$coefficients[2]
  expect_equal(coef_val[[1]], 7.350412, tolerance = 0.00001)




})
