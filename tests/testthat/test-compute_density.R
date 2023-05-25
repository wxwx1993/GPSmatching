test_that("compute_density works", {

  set.seed(223)

  a <- runif(100)
  b <- runif(100)
  a[10:12] <- NA
  cd <- compute_density(a,b)
  expect_equal(length(cd), 100)
  expect_equal(cd[10], 0.537411, tolerance = 0.001)
})
