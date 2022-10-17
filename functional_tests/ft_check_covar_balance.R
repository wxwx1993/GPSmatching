 set.seed(422)
 n <- 1000
mydata <- generate_syn_data(sample_size=n)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)

ci_appr = "matching"

if (ci_appr == "matching") {
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)

pseudo_pop <- generate_pseudo_pop(mydata$Y,
                                  mydata$treat,
                                  mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
                                  ci_appr = "matching",
                                  gps_model = "non-parametric",
                                  trim_quantiles = c(0.1,0.9),
                                  optimized_compile = TRUE,
                                  sl_lib = c("m_xgboost"),
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type = "mean",
                                  max_attempt = 1,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 1)

adjusted_corr_obj <- check_covar_balance(w = pseudo_pop$pseudo_pop[, c("w")],
                                         c = pseudo_pop$pseudo_pop[ ,pseudo_pop$covariate_cols_name, with=FALSE],
                                         counter = pseudo_pop$pseudo_pop[, c("counter_weight")],
                                         ci_appr="matching",
                                         nthread=1,
                                         covar_bl_method = "absolute",
                                         covar_bl_trs = 0.1,
                                         covar_bl_trs_type = "mean",
                                         optimized_compile=TRUE)
} else if (ci_appr == "weighting"){

  region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
  mydata$year <- as.factor(year)
  mydata$region <- as.factor(region)
  mydata$cf5 <- as.factor(mydata$cf5)

  pseudo_pop <- generate_pseudo_pop(mydata$Y,
                                    mydata$treat,
                                    mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
                                    ci_appr = "weighting",
                                    gps_model = "non-parametric",
                                    trim_quantiles = c(0.01,0.99),
                                    optimized_compile = TRUE,
                                    sl_lib = c("m_xgboost"),
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 1,
                                    scale = 0.5,
                                    nthread = 1)

  adjusted_corr_obj <- check_covar_balance_2(w = pseudo_pop$pseudo_pop[, c("w")],
                                             c = pseudo_pop$pseudo_pop[ ,pseudo_pop$covariate_cols_name, with=FALSE],
                                             counter = pseudo_pop$pseudo_pop[, c("ipw")],
                                             ci_appr="weighting",
                                             nthread=1,
                                             covar_bl_method = "absolute",
                                             covar_bl_trs = 0.1,
                                             covar_bl_trs_type = "mean")

}

