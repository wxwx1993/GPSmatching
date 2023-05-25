test_that("smooth_erf works as expected.", {
  val <- smooth_erf(matched_Y = pseudo_pop_covar_test$Y,
                    bw = 0.2,
                    matched_w = pseudo_pop_covar_test$w,
                    matched_cw = pseudo_pop_covar_test$w * 0,
                    kernel_appr = "locpol",
                    x_eval = pseudo_pop_covar_test$w)

  expect_equal(length(val), 2800)
  expect_equal(val[150], -149.4761, tolerance=0.0001)
})
