test_that("estimate_pmetric_erf works as expected", {


  # Weighting
  skip_on_cran()
  data.table::setDTthreads(1)
  set.seed(6451)
  gnm_model <-  estimate_pmetric_erf(Y ~ w + cf5,
                            family = gaussian,
                            data = pseudo_pop_weight_test,
                            verbose = FALSE,
                            model = FALSE)
  expect_equal(gnm_model$coefficients[3][[1]], 1.341568, tolerance = 0.00001)



  # Matching
  set.seed(3784)
  data_1 <- pseudo_pop_weight_test
  data_1$counter_weight <- (pseudo_pop_weight_test$counter_weight)*0
  gnm_model <-  estimate_pmetric_erf(Y ~ w + cf5,
                                     family = gaussian,
                                     data = data_1)
  expect_equal(gnm_model$coefficients[3][[1]], 2.789947, tolerance = 0.00001)




})
