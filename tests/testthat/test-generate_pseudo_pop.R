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
                                 pred_model = "sl",
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
               0.323827746927678,
               tolerance = 0.000001)


  ps_pop2 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "matching",
                                 pred_model = "sl",
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
  expect_equal(ps_pop2$adjusted_corr_results$mean_absolute_corr, 0.2145893,
               tolerance = 0.000001)

  # expect error with wrong ci_appr
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "grounding",
                                   pred_model = "sl",
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

  # expect error with wrong pred_model
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   pred_model = "fl",
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
                                   pred_model = "fl",
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

  # expect error with missing gps parameter
  expect_error(generate_pseudo_pop(mydata$Y,
                                   mydata$treat,
                                   mydata[c("cf1","cf2","cf3","cf4","cf5",
                                            "cf6","year","region")],
                                   ci_appr = "matching",
                                   pred_model = "sl",
                                   gps_model = "parametric",
                                   trim_quantiles = c(0.04,0.96),
                                   optimized_compile = TRUE,
                                   #sl_lib = c("m_xgboost"),
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                   pred_model = "sl",
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
                                 pred_model = "sl",
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
               0.493702041980042,
               tolerance = 0.000001)

  ps_pop4 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf3","cf4","cf5",
                                          "cf6","year","region")],
                                 ci_appr = "matching",
                                 pred_model = "sl",
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
  expect_true(ps_pop4$passed_covar_test)
  expect_equal(nrow(ps_pop4$pseudo_pop), 460)
  expect_equal(ps_pop4$adjusted_corr_results$mean_absolute_corr,
               0.0795775617737965,
               tolerance = 0.000001)


  expect_warning(ps_pop5 <- generate_pseudo_pop(mydata$Y,
                                 mydata$treat,
                                 mydata[c("cf1","cf2","cf4")],
                                 ci_appr = "matching",
                                 pred_model = "sl",
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
               0.05900387,
               tolerance = 0.000001)


})
