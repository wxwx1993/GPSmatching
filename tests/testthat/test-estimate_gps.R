test_that("estimate_gps works as expected.", {
  

  m_d <- gen_syn_data(sample_size = 100)
  data_with_gps_1 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  running_appr = "base",
                                  internal_use = FALSE,
                                  sl_lib = c("SL.xgboost","SL.earth","SL.gam",
                                           "SL.ranger")
                                  )
  
  data_with_gps_2 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  running_appr = "parallel",
                                  internal_use = TRUE,
                                  sl_lib = c("SL.xgboost","SL.earth","SL.gam",
                                         "SL.ranger")
                                  )
  
  expect_equal(length(data_with_gps_1),9)
  expect_equal(nrow(data_with_gps_1),100)
  expect_equal(length(data_with_gps_2),6)
})
