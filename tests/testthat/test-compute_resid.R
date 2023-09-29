test_that("Computation of residuals passes the tests.", {

  skip_on_cran()
  set.seed(156)

  a <- runif(100)
  b <- runif(100)
  c <- runif(100)
  cr <- compute_resid(a,b,c)
  expect_equal(length(cr), 100)
  expect_equal(cr[20], -0.8043024, tolerance = 0.0001)
})
