test_that("Computing min and max value of vector is correct.", {
  arr1 <- runif(100,min = 0, max = 1)
  arr2 <- runif(1000,min = 0, max = 100)
  arr2[c(10,40,18)] <- NA
  arr1_mx <- compute_min_max(arr1)
  arr2_mx <- compute_min_max(arr2)
  expect_equal(arr1_mx[1],min(arr1, na.rm = TRUE))
  expect_equal(arr1_mx[2],max(arr1, na.rm = TRUE))
  expect_equal(arr2_mx[1],min(arr2, na.rm = TRUE))
  expect_equal(arr2_mx[2],max(arr2, na.rm = TRUE))
})
