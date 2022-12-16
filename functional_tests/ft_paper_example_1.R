##      author:  Naeem Khoshnevis
##      created: August 2022
##      purpose: Reproducing examples in the paper.

# Load libraries
library(ggplot2)

# CausalGPS: Matching on Generalized Propensity Scores with Continuous Exposures
# Example 1_1: Use all data and default values.
#

data("synthetic_us_2010", package = "CausalGPS")
synthetic_us_2010$cdc_mean_bmi[synthetic_us_2010$cdc_mean_bmi > 9000] <- NA
data <- synthetic_us_2010

confounders   <- c("cs_poverty", "cs_hispanic", "cs_black", "cs_white",
                   "cs_native", "cs_asian", "cs_ed_below_highschool",
                   "cs_household_income", "cs_median_house_value",
                   "cs_total_population", "cs_other", "cs_area",
                   "cs_population_density", "cdc_mean_bmi", "cdc_pct_cusmoker",
                   "cdc_pct_sdsmoker", "cdc_pct_fmsmoker",  "cdc_pct_nvsmoker",
                   "cdc_pct_nnsmoker", "gmet_mean_tmmn", "gmet_mean_summer_tmmn",
                   "gmet_mean_winter_tmmn", "gmet_mean_tmmx",
                   "gmet_mean_summer_tmmx", "gmet_mean_winter_tmmx",
                   "gmet_mean_rmn", "gmet_mean_summer_rmn",
                   "gmet_mean_winter_rmn", "gmet_mean_rmx",
                   "gmet_mean_summer_rmx", "gmet_mean_winter_rmx",
                   "gmet_mean_sph", "gmet_mean_summer_sph",
                   "gmet_mean_winter_sph", "cms_white_pct",
                   "cms_black_pct", "cms_others_pct", "cms_hispanic_pct",
                   "cms_female_pct", "region")

confounders_s1 <- c("cs_poverty","cs_hispanic",
                    "cs_black",
                    "cs_ed_below_highschool",
                    "cs_median_house_value",
                    "cs_population_density",
                    "cdc_mean_bmi","cdc_pct_nvsmoker",
                    "gmet_mean_summer_tmmx",
                    "gmet_mean_summer_rmx",
                    "gmet_mean_summer_sph",
                    "cms_female_pct", "region"
)

# data pre-processing
data$region <- as.factor(data$region)

set.seed(172)
ps_pop_obj_1 <- generate_pseudo_pop(data$cms_mortality_pct,
                                    data$qd_mean_pm25,
                                    data.frame(data[, confounders_s1, drop=FALSE]),
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.0 ,
                                                       1.0),
                                    optimized_compile = TRUE,
                                    use_cov_transform = FALSE,
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=c(50),
                                                  xgb_eta=c(0.2)),
                                    nthread = 6,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

#pdf("example_1_1.pdf")
plot(ps_pop_obj_1)
#dev.off()

## Plotting GPS distribution

q1 <- stats::quantile(ps_pop_obj_1$pseudo_pop$w,0.25)
q2 <- stats::quantile(ps_pop_obj_1$pseudo_pop$w,0.99)

g1 <- ggplot(data = ps_pop_obj_1$pseudo_pop)
g1 <- g1 + geom_density(aes(x=w), color="blue",
                        fill="blue", bw=0.4, alpha = 0.1)
g1 <- g1 + geom_vline(xintercept = q1, linetype="dashed")
g1 <- g1 + geom_vline(xintercept = q2, linetype="dashed")
g1 <- g1 + xlab("PM2.5") + ylab("density")
g1 <- g1 + theme_bw()
plot(g1)

#pdf("example_1_1_gps_density.pdf")
plot(g1)
#dev.off()

### Example 1-2: Use trimmed data and default values

set.seed(172)
ps_pop_obj_2 <- generate_pseudo_pop(data$cms_mortality_pct,
                                    data$qd_mean_pm25,
                                    data.frame(data[, confounders_s1,
                                                            drop=FALSE]),
                                    ci_appr = "matching",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.25 ,
                                                       0.99),
                                    optimized_compile = TRUE,
                                    use_cov_transform = FALSE,
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=c(50),
                                                  xgb_eta=c(0.2)),
                                    nthread = 12,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

#pdf("example_1_2.pdf")
plot(ps_pop_obj_2)
#dev.off()

### Example 1-3: Use trimmed data and search for the best params.
# Takes 45 iteration to get 0.040 covariate balance
set.seed(637)
ps_pop_obj_3 <- generate_pseudo_pop(data$cms_mortality_pct,
                                    data$qd_mean_pm25,
                                    data.frame(data[, confounders_s1,
                                                      drop=FALSE]),
                                    ci_appr = "matching",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.25,
                                                       0.99),
                                    optimized_compile = TRUE,
                                    use_cov_transform = FALSE,
                                    transformers = list("pow2","pow3"),
                                    sl_lib = c("m_xgboost", "m_ranger", "SL.earth"),
                                    params = list(xgb_nrounds=seq(10,50,1),
                                                  xgb_eta=seq(0.1,0.4,0.01)),
                                    nthread = 12,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "maximal",
                                    max_attempt = 40,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)


pdf("example_1_3.pdf")
plot(ps_pop_obj_3)
dev.off()

summary(ps_pop_obj_3)

# Exposure response function for example_1_4

set.seed(168)
erf <- estimate_npmetric_erf(m_Y = ps_pop_obj_3$pseudo_pop$Y,
                             m_w = ps_pop_obj_3$pseudo_pop$w,
                             counter_weight = ps_pop_obj_3$pseudo_pop$counter_weight,
                             bw_seq = seq(0.2,10,0.05),
                             w_vals = seq(7,13, 0.05),
                             nthread = 12)

pdf("example_1_erf.pdf")
plot(erf, gg_labs = c("PM2.5", "All-cause Mortality"),
     gg_title = c("Exposure Response Curve"))
dev.off()

