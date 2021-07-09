test_that("estimate_gps works as expected.", {
  m_d <- generate_syn_data(sample_size = 100)
  data_with_gps_1 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  internal_use = FALSE,
                                  sl_lib = c("m_xgboost")
  )

  expect_equal(length(data_with_gps_1[[1]]),11)
  expect_equal(nrow(data_with_gps_1[[1]]),100)
})
