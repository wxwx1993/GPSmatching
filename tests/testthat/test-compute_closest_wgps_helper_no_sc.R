test_that("multiplication works", {

  set.seed(2219)
  a <- runif(10)
  b <- runif(40)
  nthread <- 1

  # Not that the (a) input for the following function should be sorted.
  a <- sort(a)

  val2 <- compute_closest_wgps_helper_no_sc(a,b,nthread)


  expect_equal(val2[1],10)
  expect_equal(val2[2],9)
  expect_equal(val2[13],1)
  expect_equal(val2[26],8)
  expect_equal(val2[30],8)


  val3 <- compute_closest_wgps_no_sc_binary_search(a, b, nthread)
  expect_true(identical(val2, val3))

})
