test_that("gpsmatching_smooth tests.", {
  
   val1 <- generate_kernel(seq(-1.1,1,0.01))
   
   expect_equal(val1[10], 0.2395511, tolerance=0.0001)
   expect_equal(val1[100], 0.396536, tolerance=0.0001)
   expect_equal(val1[150], 0.3697277, tolerance=0.0001)

   sq <- seq(0.2,2,0.2)
   val2 <- w_fun(0.25, pseudo_pop_covar_test$w, sq)

   expect_equal(length(sq),length(val2))
   expect_equal(val2[2],0.01054791, tolerance=0.0001)   

})
