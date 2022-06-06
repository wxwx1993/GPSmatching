test_that("estimate_npmetric_erf works as expected", {

  set.seed(347)
  m_d <- generate_syn_data(sample_size = 100)

  pseudo_pop <- generate_pseudo_pop(m_d$Y,
                                    m_d$treat,
                                    m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    gps_model = "non-parametric",
                                    trim_quantiles = c(0.01,0.99),
                                    optimized_compile = FALSE,
                                    sl_lib = c("SL.xgboost","SL.earth","SL.gam"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 1,
                                    scale = 0.5)

  min_w <- min(pseudo_pop$pseudo_pop$w)
  max_w <- max(pseudo_pop$pseudo_pop$w)

  res <- estimate_npmetric_erf(pseudo_pop$pseudo_pop$Y,
                               pseudo_pop$pseudo_pop$w,
                               bw_seq=seq(0.2,2,0.2),
                               w_vals=seq(min_w,max_w,0.5),
                               nthread = 1)

  expect_equal(class(res),"gpsm_erf")
  expect_equal(length(res$params$bw_seq), 10)
  expect_equal(length(res$params$w_vals), length(res$erf))
  expect_equal(res$risk_val[1], 30.71272, tolerance = 0.00001)

  # Address reproducibility issue.
  # expect_equal(res$risk_val[10], 140.09654, tolerance = 0.00001)

})
