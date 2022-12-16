test_that("Covariate balance check works as expected", {

  # Steps for regenerating the pseudo population:
  # data <- generate_syn_data(sample_size = 100,
  #                           seed = 532,
  #                           outcome_sd = 10,
  #                           gps_spec = 1,
  #                           cova_spec = 1)
  #
  # pseudo_pop_covar_test <- generate_pseudo_pop(data$Y,
  #                               data$treat,
  #                               data[c("cf1","cf2","cf3","cf4","cf5","cf6")],
  #                               ci_appr = "matching",
  #                               pred_model = "sl",
  #                               sl_lib = c("SL.xgboost","SL.earth","SL.gam"),
  #                               covar_bl_method = "absolute",
  #                               covar_bl_trs = 0.1,
  #                               max_attempt = 1,
  #                               matching_fun = "matching_l1",
  #                               delta_n = 1,
  #                               scale = 0.5)
  #
  # mydata <- generate_syn_data(sample_size = 1000)
  # #
  # year <- c(rep(c("2001"), each=200),
  #           rep(c("2002"), each=200),
  #           rep(c("2003"), each=200),
  #           rep(c("2004"), each=200),
  #           rep(c("2005"), each=200))
  # #
  # region <- rep(c(rep("North",each=50),
  #                 rep("South",each=50),
  #                 rep("East",each=50),
  #                 rep("West",each=50)), each=5)
  # #
  # mydata$year <- as.factor(year)
  # mydata$region <- as.factor(region)
  # #
  # set.seed(729)
  # pseudo_pop_weight_test <- generate_pseudo_pop(mydata$Y,
  #                                               mydata$treat,
  #                                               mydata[c("cf1","cf2","cf3",
  #                                                        "cf4","cf5","cf6",
  #                                                        "year","region")],
  #                                               trim_quantiles = c(0.0,1.0),
  #                                               ci_appr = "weighting",
  #                                               pred_model = "sl",
  #                                               sl_lib = c("m_xgboost"),
  #                                               params = list(xgb_nrounds=c(10),
  #                                                             xgb_eta=c(0.3)),
  #                                               nthread = 1,
  #                                               covar_bl_method = "absolute",
  #                                               covar_bl_trs = 0.1,
  #                                               covar_bl_trs_type = "mean",
  #                                               max_attempt = 1
  # )

  # # pseudo_pop_covar_test and pseudo_pop_weight_test are saved in R/sysdata.R
  confounders <- paste0("cf", seq(1,6))
  val1 <- check_covar_balance(w = pseudo_pop_covar_test[, c("w")],
                              c = pseudo_pop_covar_test[, confounders, with=FALSE],
                              counter_weight = pseudo_pop_covar_test[, c("w")]*0,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.3,
                              covar_bl_trs_type="mean",
                              optimized_compile = FALSE)

  expect_true(val1$pass)




  val2 <- check_covar_balance(w = pseudo_pop_covar_test[, c("w")],
                              c = pseudo_pop_covar_test[, confounders, with=FALSE],
                              counter_weight = pseudo_pop_covar_test[, c("w")]*0,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.1,
                              covar_bl_trs_type="mean",
                              optimized_compile = FALSE)

  expect_false(val2$pass)

  w_1 <- data.table(pseudo_pop_weight_test[, c("w")])
  c_1 <- data.table(pseudo_pop_weight_test[, c("cf1", "cf2", "cf3", "cf4",
                                               "cf5", "cf6", "year", "region")])
  cw <- data.table(pseudo_pop_weight_test[, c("counter_weight")])

  val3 <- check_covar_balance(w = w_1,
                              c = c_1,
                              counter_weight = cw ,
                              ci_appr = "weighting",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.1,
                              covar_bl_trs_type="mean",
                              optimized_compile = FALSE)

  expect_true(val3$pass)

  val4 <- check_covar_balance(w = w_1,
                              c = c_1,
                              counter_weight = cw,
                              ci_appr = "weighting",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.01,
                              covar_bl_trs_type="mean",
                              optimized_compile = FALSE)
  expect_false(val4$pass)
})
