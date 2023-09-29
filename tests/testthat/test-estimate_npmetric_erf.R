test_that("estimate_npmetric_erf works as expected", {
  skip_on_cran()
  set.seed(347)
  m_d <- generate_syn_data(sample_size = 400)
  m_d$id <- seq_along(1:nrow(m_d))

  pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
                                    m_d[, c("id", "cf1", "cf2", "cf3",
                                            "cf4", "cf5", "cf6")],
                                    ci_appr = "matching",
                                    gps_density = "kernel",
                                    exposure_trim_qtls = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    dist_measure = "l1",
                                    delta_n = 1,
                                    scale = 1)

  min_w <- min(pseudo_pop$pseudo_pop$w)
  max_w <- max(pseudo_pop$pseudo_pop$w)

  data <- merge(m_d[, c("id", "Y")], pseudo_pop$pseudo_pop, by="id")

  res <- estimate_npmetric_erf(data$Y,
                               data$w,
                               data$counter_weight,
                               bw_seq=seq(0.2,0.2,0.2),
                               w_vals=seq(min_w,max_w,0.5),
                               nthread = 1)

  expect_equal(class(res),"gpsm_erf")
  expect_equal(length(res$params$bw_seq), 1)
  expect_equal(length(res$params$w_vals), length(res$erf))
  #expect_equal(res$risk_val[1], 1305125, tolerance = 0.00001)
})


test_that("estimate_npmetric_erf works as expected (with earth)", {

  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(347)
  m_d <- generate_syn_data(sample_size = 400)
  m_d$id <- seq_along(1:nrow(m_d))

  pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
                                    m_d[, c("id", "cf1", "cf2", "cf3", "cf4",
                                            "cf5", "cf6")],
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    gps_density = "kernel",
                                    exposure_trim_qtls = c(0.01,0.99),
                                    sl_lib = c("SL.xgboost",
                                               "SL.earth",
                                               "SL.gam"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    dist_measure = "l1",
                                    delta_n = 1,
                                    scale = 0.5)

  min_w <- min(pseudo_pop$pseudo_pop$w)
  max_w <- max(pseudo_pop$pseudo_pop$w)

  data <- merge(m_d[, c("id", "Y")], pseudo_pop$pseudo_pop, by="id")

  res <- estimate_npmetric_erf(data$Y,
                               data$w,
                               data$counter_weight,
                               bw_seq=seq(0.2,0.4,0.2),
                               w_vals=seq(min_w,max_w,0.5),
                               nthread = 1)

  expect_equal(class(res),"gpsm_erf")
  expect_equal(length(res$params$bw_seq), 2)
  expect_equal(length(res$params$w_vals), length(res$erf))
  #expect_equal(res$risk_val[1], 1305125, tolerance = 0.00001)

})

