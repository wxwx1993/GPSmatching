test_that("matching_l1 functions as expected.", {

  set.seed(721)
  s_data <- generate_syn_data(sample_size=200,
                                   outcome_sd = 10, gps_spec = 1,
                                   cova_spec = 1)

  data_with_gps_test <- estimate_gps(s_data$Y,
                                     s_data$treat,
                                     s_data[c("cf1","cf2","cf3","cf4","cf5",
                                              "cf6")],
                                     pred_model = "sl",
                                     internal_use = TRUE,
                                     sl_lib = c("SL.xgboost","SL.earth",
                                                "SL.gam","SL.ranger")
                            )

  m_d <- data_with_gps_test

  val <- matching_l1(w = 10,
                     dataset = m_d$dataset,
                     e_gps_pred = m_d$e_gps_pred,
                     e_gps_std_pred = m_d$e_gps_std_pred,
                     w_resid = m_d$w_resid,
                     gps_mx = m_d$gps_mx,
                     w_mx = m_d$w_mx,
                     gps_model = "non-parametric",
                     delta_n = 1,
                     scale = 0.5,
                     nthread = 1)



   expect_equal(nrow(val), 6)
   expect_equal(length(val), 2)
   #expect_equal(val$row_index[2], 86, tolerance=0.00001)
   #expect_equal(val$N[4], 1, tolerance=0.00001)

})
