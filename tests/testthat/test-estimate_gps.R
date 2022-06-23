test_that("estimate_gps works as expected.", {

  set.seed(895)
  m_d <- generate_syn_data(sample_size = 100)
  data_with_gps_1 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  internal_use = FALSE,
                                  sl_lib = c("m_xgboost")
  )

  expect_equal(length(data_with_gps_1$dataset),11)
  expect_equal(nrow(data_with_gps_1$dataset),100)
  expect_equal(data_with_gps_1$dataset$gps[2], 20.991916, tolerance = 0.00001)


  data_with_gps_2 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  internal_use = TRUE,
                                  sl_lib = c("m_xgboost")
  )

  required_elements <- c( "dataset",
                          "e_gps_pred",
                          "e_gps_std_pred",
                          "w_resid",
                          "gps_mx",
                          "w_mx" )
  expect_equal(length(intersect(c(attributes(data_with_gps_2)$names),
                                required_elements)), 6L)
  expect_equal(data_with_gps_2$e_gps_pred[58,], 19.07269287,
               tolerance = 0.00001)

  # Missing values
  set.seed(1789)
  m_d_2 <- generate_syn_data(sample_size = 100)
  m_d_3 <- m_d_2
  m_d_3$treat[20] <- NA
  # Missing value in target
  # Error because SL does not support missing data.
  expect_error(estimate_gps(m_d_3$Y,
                            m_d_3$treat,
                            m_d_3[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                            pred_model = "sl",
                            internal_use = TRUE,
                            sl_lib = c("m_xgboost"))
               )

})


