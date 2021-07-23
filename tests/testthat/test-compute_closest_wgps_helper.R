test_that("compute_closest_wgps_helper works as expected.", {

  set.seed(2897)
  a <- c(0.5,3,10,0.7)
  b <- c(1,2,16,0.5)
  c_m_d <- c(0.1,0.1,0.1,0.1)
  sc <- 0.5
  val <- compute_closest_wgps_helper(a,b,c_m_d,sc)
  expect_equal(val[1],4)
  expect_equal(val[2],2)
  expect_equal(val[3],3)
  expect_equal(val[4],1)
})
