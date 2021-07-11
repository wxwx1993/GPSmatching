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
  #
  # year <- c(rep(c("2001"), each=200),
  #           rep(c("2002"), each=200),
  #           rep(c("2003"), each=200),
  #           rep(c("2004"), each=200),
  #           rep(c("2005"), each=200))
  #
  # region <- rep(c(rep("North",each=50),
  #                 rep("South",each=50),
  #                 rep("East",each=50),
  #                 rep("West",each=50)), each=5)
  #
  # mydata$year <- as.factor(year)
  # mydata$region <- as.factor(region)
  #
  # pseudo_pop_weight_test <- generate_pseudo_pop(mydata$Y,
  #                                               mydata$treat,
  #                                               mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
  #                                               ci_appr = "weighting",
  #                                               pred_model = "sl",
  #                                               sl_lib = c("m_xgboost"),
  #                                               params = list(xgb_nrounds=c(10),
  #                                                             xgb_eta=c(0.3)),
  #                                               nthread = 1,
  #                                               covar_bl_method = "absolute",
  #                                               covar_bl_trs = 0.1,
  #                                               max_attempt = 1
  # )

  # # pseudo_pop_covar_test is saved in R/sysdata.R

  val1 <- check_covar_balance(pseudo_pop = pseudo_pop_covar_test,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.3,
                              optimized_compile = FALSE)

  expect_true(val1$pass)

  val2 <- check_covar_balance(pseudo_pop = pseudo_pop_covar_test,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.1,
                              optimized_compile = FALSE)

  expect_false(val2$pass)

  # temp solution.
  # TODO: regenrate pseudo_pop_weight_test to include counter and row_index.
  mydata <- data.frame(pseudo_pop_weight_test[,c("Y","w","gps","gps","gps",
                                                 "ipw","cf1","cf2","cf3",
                                                 "cf4","cf5","cf6",
                                                 "year","region")])
  setDT(mydata)
  val3 <- check_covar_balance(pseudo_pop = mydata,
                              ci_appr = "weighting",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.1,
                              optimized_compile = FALSE)

  expect_false(val3$pass)

  val4 <- check_covar_balance(pseudo_pop = mydata,
                              ci_appr = "weighting",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.12,
                              optimized_compile = FALSE)
  expect_true(val4$pass)

})
