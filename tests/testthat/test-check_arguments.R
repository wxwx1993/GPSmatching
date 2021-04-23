test_that("Check arguments works as expected.", {
  expect_error(check_args())
  expect_error(check_args_estimate_gps())
  expect_error(check_args_compile_pseudo_pop())

  val1 <- check_args(pred_model = "sl",
                     running_appr = "base",
                     use_cov_transform = FALSE,
                     transformers = list(),
                     sl_lib=c("xyz"),
                     covar_bl_method="absolute",
                     ci_appr = "matching",
                     gps_model = "non-parametric",
                     covar_bl_trs=0.1,
                     max_attempt=2,
                     matching_fun="matching_l1",
                     delta_n=0.2,
                     scale=0.5)
  expect_true(val1)

  val2 <- check_args_estimate_gps(pred_model = "sl",
                                  running_appr = "base",
                                  gps_model = "non-parametric",
                                  sl_lib=c("xgboost"))
  expect_true(val2)

  val3 <- check_args_compile_pseudo_pop(ci_appr = "matching",
                                        covar_bl_method="absolute",
                                        covar_bl_trs=1,
                                        max_attempt=10,
                                        matching_fun="matching_l1",
                                        delta_n=1,
                                        scale=0.5)
  expect_true(val3)

})
