test_that("compute_closest_wgps works fine.", {

  set.seed(187)
  a <- 1:10
  b <- 1:5
  c <- (1:10)*0.1
  d <- 4
  sc <- 0.5
  nthread <- 1

  wm <- compute_closest_wgps(a,b,c,d,sc,nthread)

  expect_equal(length(wm), 5)
  expect_equal(wm[2], 2)
  expect_error(compute_closest_wgps(a,a,a,a,sc,nthread))
  expect_error(compute_closest_wgps(a,b,c,d,a,nthread))
})
