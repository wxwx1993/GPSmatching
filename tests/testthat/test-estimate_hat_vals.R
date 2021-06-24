test_that("estimate_hat_vals works as expected.", {
  
  val <- estimate_hat_vals(0.2, data_with_gps_test[[2]], seq(1,10,1))

  expect_equal(length(val),200)
  expect_equal(val[20], 0.1250456, tolerance=0.0001)

})
