test_that("create_weighting works as expected.", {

  set.seed(481)
  m_d <- generate_syn_data(sample_size = 100)
  pseudo_pop <- generate_pseudo_pop(m_d$Y,
                                    m_d$treat,
                                    m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                    ci_appr = "matching",
                                    gps_model = "non-parametric",
                                    trim_quantiles = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 1,
                                    scale = 0.5)

  dataset <- pseudo_pop$pseudo_pop
  dataset1 <- dataset[, !c("w")]

  # expect error if there is no column with "w"
  expect_error(create_weighting(dataset = dataset1))


  expect_false(pseudo_pop$passed_covar_test)
  expect_equal(length(pseudo_pop$pseudo_pop), 11)
  expect_equal(nrow(pseudo_pop$pseudo_pop),98)
  expect_equal(mean(pseudo_pop$pseudo_pop$Y), -37.32878, tolerance = 0.0001)
})


test_that("create_weighting works as expected.", {

  skip_if_not_installed("earth")

  set.seed(481)
  m_d <- generate_syn_data(sample_size = 100)
  pseudo_pop <- generate_pseudo_pop(m_d$Y,
                                    m_d$treat,
                                    m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                    ci_appr = "matching",
                                    gps_model = "non-parametric",
                                    trim_quantiles = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost","SL.earth","SL.gam"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 1,
                                    scale = 0.5)

  dataset <- pseudo_pop$pseudo_pop
  dataset1 <- dataset[, !c("w")]

  expect_error(create_weighting(dataset = dataset1))

  expect_false(pseudo_pop$passed_covar_test)
  expect_equal(length(pseudo_pop$pseudo_pop), 11)
  expect_equal(nrow(pseudo_pop$pseudo_pop), 98)
  expect_equal(mean(pseudo_pop$pseudo_pop$Y), -37.32878, tolerance = 0.0001)
})

