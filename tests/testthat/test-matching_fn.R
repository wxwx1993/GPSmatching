test_that("matching_l1 functions as expected.", {

  set.seed(721)
  s_data <- generate_syn_data(sample_size=200,
                              outcome_sd = 10,
                              gps_spec = 1,
                              cova_spec = 1)

  s_data$id <- seq_along(1:nrow(s_data))

  data_with_gps_test <- estimate_gps(s_data[, c("id", "w")],
                                     s_data[, c("id", "cf1", "cf2", "cf3",
                                                "cf4","cf5", "cf6")],
                                     sl_lib = c("SL.xgboost","SL.earth",
                                                "SL.gam","SL.ranger")
                            )

  m_d <- data_with_gps_test

  val <- matching_fn(w = 10,
                     dataset = m_d$dataset,
                     exposure_col_name = c("w"),
                     e_gps_pred = m_d$dataset$e_gps_pred,
                     e_gps_std_pred = m_d$dataset$e_gps_std_pred,
                     w_resid = m_d$dataset$w_resid,
                     gps_mx = m_d$gps_mx,
                     w_mx = m_d$w_mx,
                     dist_measure = "l1",
                     gps_density = "kernel",
                     delta_n = 1,
                     scale = 0.5,
                     nthread = 1)



   expect_equal(nrow(val), 6)
   expect_equal(length(val), 2)
})
