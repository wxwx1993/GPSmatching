test_that("Compiling pseudo pop works as expected.", {

  set.seed(509)
  m_d <- generate_syn_data(sample_size = 100)
  data_with_gps_1 <- estimate_gps(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  pred_model = "sl",
                                  internal_use = TRUE,
                                  sl_lib = c("m_xgboost"))


  # Wrong ci_appr
  expect_error(compile_pseudo_pop(dataset=data_with_gps_1, ci_appr="grounding",
                                   gps_model = "parametric", bin_seq,
                                   nthread = 1,
                                   trim_quantiles = c(0.01, 0.99),
                                   optimized_compile = TRUE))


  #
  set.seed(509)
  pseudo_pop_1 <- compile_pseudo_pop(dataset=data_with_gps_1,
                                     ci_appr="matching",
                                     gps_model="parametric",
                                     bin_seq = NULL,
                                     nthread = 1,
                                     trim_quantiles = c(0.01, 0.99),
                                     optimized_compile = TRUE,
                                     covar_bl_method = "absolute",
                                     covar_bl_trs = 0.1,
                                     covar_bl_trs_type = "mean",
                                     max_attempt = 4,
                                     matching_fun = "matching_l1",
                                     delta_n = 1,
                                     scale = 0.5)


  expect_equal(sum(pseudo_pop_1$counter), 2500)
  expect_equal(nrow(pseudo_pop_1),100)
  expect_equal(length(pseudo_pop_1),11)

})
