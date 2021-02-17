test_that("Covariate balance check works as expected", {

  # Steps for regenerating the pseudo population:
  # data <- gen_syn_data(sample_size = 100,
  #                      seed = 532,
  #                      outcome_sd = 10,
  #                      gps_spec = 1,
  #                      cova_spec = 1)
  #
  # pseuodo_pop_covar_test <- gen_pseudo_pop(data$Y,
  #                               data$treat,
  #                               data[c("cf1","cf2","cf3","cf4","cf5","cf6")],
  #                               ci_appr = "matching",
  #                               running_appr = "base",
  #                               pred_model = "sl",
  #                               sl_lib = c("SL.xgboost","SL.earth","SL.gam"),
  #                               covar_bl_method = "absolute",
  #                               covar_bl_trs = 0.1,
  #                               max_attempt = 1,
  #                               matching_fun = "matching_l1",
  #                               delta_n = 1,
  #                               scale = 0.5)

  # pseudo_pop_covar_test is saved in R/sysdata.R

  val1 <- check_covar_balance(pseudo_pop = pseudo_pop_covar_test,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.3)

  expect_true(val1)

  val2 <- check_covar_balance(pseudo_pop = pseudo_pop_covar_test,
                              ci_appr = "matching",
                              covar_bl_method="absolute",
                              covar_bl_trs=0.1)

  expect_false(val2)

})
