test_that("create_weighting works as expected.", {

  skip_on_cran()
  set.seed(481)
  data.table::setDTthreads(1)
  m_d <- generate_syn_data(sample_size = 100)
  pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
                                    m_d[, c("id", "cf1", "cf2", "cf3", "cf4",
                                            "cf5", "cf6")],
                                    ci_appr = "weighting",
                                    gps_density = "kernel",
                                    exposure_trim_qtls = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1)

  dataset <- pseudo_pop$pseudo_pop
  dataset1 <- dataset
  dataset1$w <- NULL


  # expect error if there is no column with "w"
  expect_error(create_weighting(dataset = dataset1))


  expect_false(pseudo_pop$passed_covar_test)
  expect_equal(length(pseudo_pop$pseudo_pop), 10)
  expect_equal(nrow(pseudo_pop$pseudo_pop),98)
})


test_that("create_weighting works as expected.", {

  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(481)
  m_d <- generate_syn_data(sample_size = 100)
  pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
                                    m_d[, c("id", "cf1", "cf2", "cf3",
                                            "cf4", "cf5", "cf6")],
                                    ci_appr = "weighting",
                                    gps_density = "kernel",
                                    exposure_trim_qtls = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost",
                                               "SL.earth",
                                               "SL.gam"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1)

  dataset <- pseudo_pop$pseudo_pop
  dataset1 <- dataset
  dataset1$w <- NULL

  expect_error(create_weighting(dataset = dataset1))

  expect_true(pseudo_pop$passed_covar_test)
  expect_equal(length(pseudo_pop$pseudo_pop), 10)
  expect_equal(nrow(pseudo_pop$pseudo_pop), 98)
})

