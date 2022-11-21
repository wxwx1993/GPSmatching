##      author:  Naeem Khoshnevis
##      created: October 2022
##      purpose: Reproducing examples in https://nsaph-software.github.io/intro.html.

# Load libraries
library(ggplot2)

# CausalGPS: Matching on Generalized Propensity Scores with Continuous Exposures
# Example 1: Use all data and default values.
#

data("synthetic_us_2010", package = "CausalGPS")
synthetic_us_2010$cdc_mean_bmi[synthetic_us_2010$cdc_mean_bmi > 9000] <- NA
data <- synthetic_us_2010

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

set.seed(574)
ps_pop_obj_1 <- generate_pseudo_pop(data$cms_mortality_pct,
                                    data$qd_mean_pm25,
                                    data.frame(data[, confounders_s1, drop=FALSE]),
                                    ci_appr = "matching",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.25 ,
                                                       0.99),
                                    optimized_compile = TRUE,
                                    use_cov_transform = TRUE,
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=seq(10,60),
                                                  xgb_eta=seq(0.04, 0.4, 0.02)),
                                    nthread = 12,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "maximal",
                                    max_attempt = 10,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

pdf("example_w_1_covar.pdf")
plot(ps_pop_obj_1)
dev.off()

summary(ps_pop_obj_1)

# Exposure response function for example_1_4

set.seed(168)
erf <- estimate_npmetric_erf(m_Y = ps_pop_obj_1$pseudo_pop$Y,
                             m_w = ps_pop_obj_1$pseudo_pop$w,
                             counter_weight = ps_pop_obj_1$pseudo_pop$counter_weight,
                             bw_seq = seq(0.2,10,0.05),
                             w_vals = seq(7,13, 0.05),
                             nthread = 12)

pdf("example_w_1_erf.pdf")
plot(erf, gg_labs = c("PM2.5", "All-cause Mortality"),
     gg_title = c("Exposure Response Curve"))
dev.off()

