set.seed(69754)
m_d <- generate_syn_data(sample_size = 2000)
pseudo_pop <- generate_pseudo_pop(m_d$Y,
                                 m_d$treat,
                                 m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                 ci_appr = "matching",
                                 pred_model = "sl",
                                 sl_lib = c("m_xgboost"),
                                 params = list(xgb_nrounds=c(10,20,30),
                                  xgb_eta=c(0.1,0.2,0.3)),
                                 nthread = 4,
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type="mean",
                                 optimized_compile = TRUE,
                                 max_attempt = 1,
                                 matching_fun = "matching_l1",
                                 delta_n = 1,
                                 scale = 0.5)

erf_obj <- estimate_npmetric_erf(pseudo_pop$pseudo_pop$Y,
                                 pseudo_pop$pseudo_pop$w,
                                 pseudo_pop$pseudo_pop$counter,
                                 bw_seq=seq(0.5,2,0.5),
                                 w_vals = seq(2,20,0.5),
                                 nthread = 1)
