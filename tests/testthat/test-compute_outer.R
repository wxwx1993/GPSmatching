test_that("ComputeOuter function works fine.", {

  a1 <- 1:10
  a2 <- 1:20
  r1 <- compute_outer(a1,a2,'-')

  set.seed(892)
  b1 <- runif(200)
  b2 <- runif(500)
  r2 <- compute_outer(b1,b2,'-')

  expect_equal(dim(r1)[1], 10)
  expect_equal(dim(r1)[2], 20)
  expect_equal(r1[9,20],11)

  expect_equal(r2[78,120], 0.1134655, tolerance = 0.0001)
})
