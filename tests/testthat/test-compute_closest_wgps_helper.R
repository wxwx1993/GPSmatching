test_that("compute_closest_wgps_helper works as expected.", {

  a <- c(0.5,3,10,0.7)
  b <- c(1,2,16,0.5)
  c_m_d <- c(0.1,0.1,0.1,0.1)
  sc <- 0.5
  nthread <- 1
  val <- compute_closest_wgps_helper(a,b,c_m_d,sc,nthread)
  expect_equal(val[1],4)
  expect_equal(val[2],2)
  expect_equal(val[3],3)
  expect_equal(val[4],1)

  a <- c(0.5,3,10,0.7)
  b <- c(1, 2, 16, 8, 0.5, 3)
  c_m_d <- c(0.1,0.1,0.1,0.1)
  sc <- 0.5
  val2 <- compute_closest_wgps_helper(a,b,c_m_d,sc,nthread)
  expect_equal(val2[1],4)
  expect_equal(val2[2],2)
  expect_equal(val2[3],3)
  expect_equal(val2[4],3)
  expect_equal(val2[5],1)
  expect_equal(val2[6],2)

  a <- c(1,0.5,5,4,10,21,4.1,24,9,3)
  b <- c(1, 16, 4.2,9,3.05)
  c_m_d <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
  sc <- 0.5
  val3 <- compute_closest_wgps_helper(a,b,c_m_d,sc,nthread)
  expect_equal(val3[1],1)
  expect_equal(val3[2],6)
  expect_equal(val3[3],7)

  set.seed(2897)
  a <- runif(10)
  b <- runif(20)
  c_m_d <- rep(0.1,each=10)
  sc <- 0.5
  val1 <- compute_closest_wgps_helper(a,b,c_m_d,sc,nthread)
  expect_equal(val1[1],8)
  expect_equal(val1[5],3)
  expect_equal(val1[10],9)
  expect_equal(val1[15],4)
  expect_equal(val1[20],1)

  set.seed(321)
  a <- runif(10)
  b <- runif(40)
  c_m_d <- rep(0.1,each=10)
  sc <- 0.5
  val2 <- compute_closest_wgps_helper(a,b,c_m_d,sc,nthread)
  expect_equal(val2[1],7)
  expect_equal(val2[10],10)
  expect_equal(val2[13],2)
  expect_equal(val2[26],9)
  expect_equal(val2[30],7)
})
