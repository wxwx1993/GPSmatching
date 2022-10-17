##      author:  Naeem Khoshnevis
##      created: October 2022
##      purpose: Reproducing example 2 in the paper.

# Load libraries
library(ggplot2)

# CausalGPS: Matching on Generalized Propensity Scores with Continuous Exposures
# Example 2: Use all data and default values.
#

data("synthetic_us_2010", package = "CausalGPS")

# cdc_mean_bmi larger that 9000 is not a real data.
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

# data preprocessing
data$region <- as.factor(data$region)

log1 <- function(x){log(abs(x)+0.001)}

set.seed(369)
set_logger(logger_level = "DEBUG")
ps_pop_obj_1 <- generate_pseudo_pop(data$cms_mortality_pct,
                                    data$qd_mean_pm25,
                                    data.frame(data[, confounders_s1,
                                                            drop=FALSE]),
                                    ci_appr = "weighting",
                                    gps_model = "non-parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.25 ,
                                                       0.95),
                                    optimized_compile = TRUE,
                                    use_cov_transform = FALSE,
                                    transformers = list("pow2","pow3"),
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=seq(5,50,1),
                                                  xgb_eta=seq(0.1,0.5,0.01)),
                                    nthread = 12,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "maximal",
                                    max_attempt = 10,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

plot(ps_pop_obj_1)


# pdf("example_2_1.pdf")
# plot(ps_pop_obj_1)
# dev.off()

set.seed(168)
erf <- estimate_npmetric_erf(matched_Y = ps_pop_obj_1$pseudo_pop$Y,
                             matched_w = ps_pop_obj_1$pseudo_pop$w,
                             matched_counter = NULL,
                             bw_seq = seq(0.5,4,0.1),
                             w_vals = seq(7,13, 0.05),
                             nthread = 12)

# pdf("example_1_erf.pdf")
# plot(erf, gg_labs = c("PM2.5", "All-cause Mortality"),
#      gg_title = c("Exposure Response Curve"))
# dev.off()
