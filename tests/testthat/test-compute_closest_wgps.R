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


  # with or without sc should give the same result for the following inputs.
  set.seed(3697)
  a <- 1:10
  b <- 1:5
  c <- (1:10)*0.0
  d <- 0

  wm_no_sc <- compute_closest_wgps(a = a,
                                   b = b,
                                   c = c,
                                   d = d,
                                   sc = 1,
                                   nthread = 1)

  wm_with_sc <- compute_closest_wgps(a = a,
                                     b = b,
                                     c = c,
                                     d = d,
                                     sc = 0.9999,
                                     nthread = 1)

  expect_true(identical(wm_no_sc, wm_with_sc))

  set.seed(767)
  a <- runif(50)
  b <- runif(10)
  c <- a*0.0
  d <- 0

  wm_no_sc <- compute_closest_wgps(a = a,
                                   b = b,
                                   c = c,
                                   d = d,
                                   sc = 1,
                                   nthread = 1)

  wm_with_sc <- compute_closest_wgps(a = a,
                                     b = b,
                                     c = c,
                                     d = d,
                                     sc = 0.9999,
                                     nthread = 1)

  expect_true(identical(wm_no_sc, wm_with_sc))
})
