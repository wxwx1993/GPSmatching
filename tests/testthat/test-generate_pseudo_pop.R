test_that("generate_pseudo_pop works as expected.", {

  set.seed(4321)
  n <- 500
  mydata <- generate_syn_data(sample_size=n)
  year <- sample(x=c("2001","2002","2003","2004","2005"),
                 size = n, replace = TRUE)
  region <- sample(x=c("North", "South", "East", "West"),
                   size = n, replace = TRUE)

  mydata$year <- as.factor(year)
  mydata$region <- as.factor(region)
  mydata$cf5 <- as.factor(mydata$cf5)


  ps_pop1 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "matching",
                                 gps_model = "non-parametric",
                                 trim_quantiles = c(0.01,0.99),
                                 optimized_compile = TRUE,
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 1,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1)

  expect_equal(class(ps_pop1),"gpsm_pspop")
  expect_false(ps_pop1$passed_covar_test)
  expect_equal(nrow(ps_pop1$pseudo_pop), 490)
  expect_equal(ps_pop1$adjusted_corr_results$mean_absolute_corr,
               0.2580037,
               tolerance = 0.000001)

  # Test if all required attributes are included in the final object.
  expect_true(("params" %in% names(ps_pop1)))
  expect_true(("pseudo_pop" %in% names(ps_pop1)))
  expect_true(("adjusted_corr_results" %in% names(ps_pop1)))
  expect_true(("original_corr_results" %in% names(ps_pop1)))
  expect_true(("fcall" %in% names(ps_pop1)))
  expect_true(("passed_covar_test" %in% names(ps_pop1)))
  expect_true(("counter" %in% names(ps_pop1)))
  expect_true(("ci_appr" %in% names(ps_pop1)))
  expect_true(("optimized_compile" %in% names(ps_pop1)))
  expect_true(("best_gps_used_params" %in% names(ps_pop1)))
  expect_true(("covariate_cols_name" %in% names(ps_pop1)))

  ps_pop2 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "matching",
                                 gps_model = "parametric",
                                 trim_quantiles = c(0.04,0.96),
                                 optimized_compile = TRUE,
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 1,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1)

  expect_equal(class(ps_pop2),"gpsm_pspop")
  expect_false(ps_pop2$passed_covar_test)
  expect_equal(nrow(ps_pop2$pseudo_pop), 460)
  expect_equal(ps_pop2$adjusted_corr_results$mean_absolute_corr,
               0.2243034,
               tolerance = 0.000001)

  # expect error with wrong ci_appr
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "grounding",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))

  # expect error with wrong gps_model
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "half-parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))

  # expect error with wrong max attempt
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = "five",
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))

  # expect error with wrong optimized compile answer
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = "YES",
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))

  # expect error with wrong covar_bl_method
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "nonabsolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))


  # expect error with wrong scale
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 1.5,
                                   nthread = 1))

  #expect error with wrong answer in using cove transform.
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   use_cov_transform = "YES",
                                   transformers = list("pow2","pow3"),
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 4,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))


  #expect error with wrong transformers.
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   use_cov_transform = TRUE,
                                   transformers = numeric(),
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   max_attempt = 4,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))


  # expect error with missing parameter
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   use_cov_transform = TRUE,
                                   sl_lib = c("m_xgboost"),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   max_attempt = 1,
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5,
                                   nthread = 1))

  # Test on weighting
  ps_pop3 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "weighting",
                                 gps_model = "parametric",
                                 trim_quantiles = c(0.04,0.96),
                                 optimized_compile = TRUE,
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 1,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1)

  expect_equal(class(ps_pop3),"gpsm_pspop")
  expect_false(ps_pop3$passed_covar_test)
  expect_equal(nrow(ps_pop3$pseudo_pop), 460)
  expect_equal(ps_pop3$adjusted_corr_results$mean_absolute_corr,
               0.4481222,
               tolerance = 0.000001)

  ps_pop4 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "matching",
                                 gps_model = "parametric",
                                 trim_quantiles = c(0.04,0.96),
                                 optimized_compile = TRUE,
                                 use_cov_transform = TRUE,
                                 transformers = list("pow2","pow3"),
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 4,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1)

  expect_equal(class(ps_pop4),"gpsm_pspop")
  expect_false(ps_pop4$passed_covar_test)
  expect_equal(nrow(ps_pop4$pseudo_pop), 460)
  expect_equal(ps_pop4$adjusted_corr_results$mean_absolute_corr,
               0.2210705,
               tolerance = 0.000001)


  expect_warning(ps_pop5 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf4")],
                                 ci_appr = "matching",
                                 gps_model = "parametric",
                                 trim_quantiles = c(0.04,0.96),
                                 optimized_compile = TRUE,
                                 use_cov_transform = TRUE,
                                 transformers = list("pow2","pow3"),
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.02,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 7,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1))

  expect_equal(class(ps_pop5),"gpsm_pspop")
  expect_false(ps_pop5$passed_covar_test)
  expect_equal(nrow(ps_pop4$pseudo_pop), 460)
  expect_equal(ps_pop5$adjusted_corr_results$mean_absolute_corr,
               0.1078871,
               tolerance = 0.000001)


})
