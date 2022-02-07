test_that("Check arguments works as expected.", {
  expect_error(check_args())
  expect_error(check_args_estimate_gps())
  expect_error(check_args_compile_pseudo_pop())

  val1 <- check_args(pred_model = "sl",
                     use_cov_transform = FALSE,
                     transformers = list(),
                     sl_lib=c("xyz"),
                     trim_quantiles = c(0.01,0.99),
                     optimized_compile = FALSE,
                     covar_bl_method="absolute",
                     ci_appr = "matching",
                     gps_model = "non-parametric",
                     covar_bl_trs=0.1,
                     covar_bl_trs_type="mean",
                     max_attempt=2,
                     matching_fun="matching_l1",
                     delta_n=0.2,
                     scale=0.5)
  expect_true(val1)

  val2 <- check_args_estimate_gps(pred_model = "sl",
                                  gps_model = "non-parametric",
                                  sl_lib=c("xgboost"))
  expect_true(val2)

  val3 <- check_args_compile_pseudo_pop(ci_appr = "matching",
                                        covar_bl_method="absolute",
                                        covar_bl_trs=1,
                                        covar_bl_trs_type="mean",
                                        max_attempt=10,
                                        trim_quantiles = c(0.01,0.99),
                                        optimized_compile = FALSE,
                                        matching_fun="matching_l1",
                                        delta_n=1,
                                        scale=0.5)
  expect_true(val3)

  # trim_quantiles should be in [0,1] range and first element less than
  # the second element.
  expect_error(
    check_args_compile_pseudo_pop(ci_appr = "matching",
                                  covar_bl_method="absolute",
                                  covar_bl_trs=1,
                                  covar_bl_trs_type="mean",
                                  max_attempt=10,
                                  trim_quantiles = c(1,2),
                                  optimized_compile = FALSE,
                                  matching_fun="matching_l1",
                                  delta_n=1,
                                  scale=0.5)

  )

  # trim_quantiles should be numeric values.
  expect_error(
    check_args_compile_pseudo_pop(ci_appr = "matching",
                                  covar_bl_method="absolute",
                                  covar_bl_trs=1,
                                  covar_bl_trs_type="mean",
                                  max_attempt=10,
                                  trim_quantiles = c("a","b"),
                                  optimized_compile = FALSE,
                                  matching_fun="matching_l1",
                                  delta_n=1,
                                  scale=0.5)

  )

  # "min is not accepted covar_bl_trs_type"
  expect_error(
    check_args_compile_pseudo_pop(ci_appr = "matching",
                                  covar_bl_method="absolute",
                                  covar_bl_trs=1,
                                  covar_bl_trs_type="min",
                                  max_attempt=10,
                                  trim_quantiles = c("a","b"),
                                  optimized_compile = FALSE,
                                  matching_fun="matching_l1",
                                  delta_n=1,
                                  scale=0.5)

  )

})
