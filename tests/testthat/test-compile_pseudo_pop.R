test_that("Compiling pseudo pop works as expected.", {

  set.seed(509)
  m_d <- generate_syn_data(sample_size = 100)
  gps_obj <- estimate_gps(m_d$Y,
                          m_d$treat,
                          m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                          pred_model = "sl",
                          internal_use = TRUE,
                          sl_lib = c("m_xgboost"))


  # Wrong ci_appr
  expect_error(compile_pseudo_pop(dataset=gps_obj$dataset, ci_appr="grounding",
                                   gps_model = "parametric", bin_seq,
                                   nthread = 1,
                                   trim_quantiles = c(0.01, 0.99),
                                   optimized_compile = TRUE))


  #
  set.seed(509)
  pseudo_pop_1 <- compile_pseudo_pop(data_obj = gps_obj,
                                     ci_appr = "matching",
                                     gps_model = "parametric",
                                     bin_seq = NULL,
                                     nthread = 1,
                                     trim_quantiles = c(0.01, 0.99),
                                     optimized_compile = TRUE,
                                     covar_bl_method = "absolute",
                                     covar_bl_trs = 0.1,
                                     covar_bl_trs_type = "mean",
                                     matching_fun = "matching_l1",
                                     delta_n = 1,
                                     scale = 0.5)


  expect_equal(sum(pseudo_pop_1$counter_weight), 2500)
  expect_equal(nrow(pseudo_pop_1),100)
  expect_equal(length(pseudo_pop_1),11)




  set.seed(934)
  #data <- list(pseudo_pop_weight_test[, !c("counter_weight")])
  data <- list(pseudo_pop_weight_test)
  obj <- list()
  class(obj) <- "cgps_gps"
  obj$dataset <- data[[1]]
  pseudo_pop_2 <- compile_pseudo_pop(data_obj = obj,
                                     ci_appr="weighting",
                                     gps_model="parametric",
                                     bin_seq = NULL,
                                     nthread = 1,
                                     trim_quantiles = c(0.01, 0.99),
                                     optimized_compile = TRUE,
                                     covar_bl_method = "absolute",
                                     covar_bl_trs = 0.1,
                                     covar_bl_trs_type = "mean",
                                     delta_n = 1,
                                     scale = 0.5)



  expect_equal(nrow(pseudo_pop_2),1000)
  expect_equal(length(pseudo_pop_2),13)
  expect_equal(mean(pseudo_pop_2$counter_weight),
               0.7465975, tolerance = 0.00001)


})
