set.seed(509)
m_d <- generate_syn_data(sample_size = 1000)
gps_obj <- estimate_gps(m_d[, c("id", "w")],
                        m_d[, c("id", "cf1", "cf2", "cf3",
                                "cf4","cf5","cf6")],
                        sl_lib = c("m_xgboost"))

set.seed(509)
pseudo_pop_1 <- compile_pseudo_pop(data_obj = gps_obj,
                                   ci_appr = "matching",
                                   gps_density = "normal",
                                   bin_seq = NULL,
                                   exposure_col_name = c("w"),
                                   nthread = 1,
                                   trim_quantiles = c(0.01, 0.99),
                                   covar_bl_method = "absolute",
                                   covar_bl_trs = 0.1,
                                   covar_bl_trs_type = "mean",
                                   matching_fun = "matching_l1",
                                   delta_n = 1,
                                   scale = 0.5)

