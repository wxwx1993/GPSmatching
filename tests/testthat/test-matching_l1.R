test_that("matching_l1 functions as expected.", {

  #  s_data <- generate_syn_data(sample_size=200, seed = 721,
  #                         outcome_sd = 10, gps_spec = 1,
  #                         cova_spec = 1)
  #
  #  data_with_gps_test <- estimate_gps(s_data$Y,
  #                                     s_data$treat,
  #                                     s_data[c("cf1","cf2","cf3","cf4","cf5",
  #                                              "cf6")],
  #                                     pred_model = "sl",
  #                                     internal_use = TRUE,
  #                                     sl_lib = c("SL.xgboost","SL.earth",
  #                                                "SL.gam","SL.ranger")
  #                            )

  m_d <- data_with_gps_test
  val <- matching_l1(w = 10,
                     m_d[[1]],
                     m_d[[2]],
                     m_d[[3]],
                     m_d[[4]],
                     m_d[[5]],
                     m_d[[6]],
                     gps_model = "non-parametric",
                     delta_n=1,
                     scale=0.5,
                     optimized_compile = FALSE)

   expect_equal(nrow(val),200)
   expect_equal(length(val),9)
   expect_equal(val$cf1[10],-0.8642621, tolerance=0.00001)

})
