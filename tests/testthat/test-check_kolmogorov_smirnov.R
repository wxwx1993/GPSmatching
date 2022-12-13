test_that("check_kolmogorov_smirnov works as expected.", {
  set.seed(8422)
  n <- 200
  mydata <- generate_syn_data(sample_size=n)
  year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
  region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
  mydata$year <- as.factor(year)
  mydata$region <- as.factor(region)
  mydata$cf5 <- as.factor(mydata$cf5)

  pseudo_pop <- generate_pseudo_pop(mydata$Y,
                                    mydata$treat,
                                    mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
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

  output <- CausalGPS:::check_kolmogorov_smirnov(w = pseudo_pop$pseudo_pop[, c("w")],
                                                 c = pseudo_pop$pseudo_pop[ ,
                                                                            pseudo_pop$covariate_cols_name,
                                                                            with=FALSE],
                                                 counter = pseudo_pop$pseudo_pop[, c("counter_weight")],
                                                 ci_appr="matching",
                                                 nthread=1,
                                                 optimized_compile=TRUE)

  expect_equal(length(output), 2L)
  expect_equal(length(output$ks_stat),9L)
  expect_equal(length(output$stat_vals), 3L)
  expect_equal(output$ks_stat[["w"]], 0.1098639, tolerance = 0.000001)
  expect_equal(output$ks_stat[["cf3"]], 0.1319728, tolerance = 0.000001)
  expect_equal(output$stat_vals[["maximal_val"]], 0.1931973, tolerance = 0.000001)
})
