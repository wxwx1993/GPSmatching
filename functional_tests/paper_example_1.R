##      author:  Naeem Khoshnevis
##      created: June 2023
##      purpose: Reproducing examples in the paper.

# Load libraries
library(ggplot2)
library(CausalGPS)

set_logger(logger_level = "TRACE")

# CausalGPS: Matching on Generalized Propensity Scores with Continuous Exposures

# Input parameters -------------------------------------------------------------
sc_name <- "ex1"
ci_appr <- "weighting"
gps_model <- "normal"
# ------------------------------------------------------------------------------

# Load data --------------------------------------------------------------------
load("zip_data.RData")
data.table::setDF(zip_data)
data <- zip_data

# Select confounders -----------------------------------------------------------
confounders   <- c("zip", "year", "mean_bmi", "smoke_rate",
                   "hispanic", "pct_blk", "medhouseholdincome",
                   "medianhousevalue", "poverty", "popdensity",
                   "pct_owner_occ", "summer_tmmx", "winter_tmmx",
                   "summer_rmax", "winter_rmax", "regionNORTHEAST",
                   "regionSOUTH", "regionWEST")

confounders_1   <- c("mean_bmi", "smoke_rate",
                     "hispanic", "pct_blk", "medhouseholdincome",
                     "medianhousevalue", "poverty", "popdensity",
                     "pct_owner_occ", "summer_tmmx", "winter_tmmx",
                     "summer_rmax", "winter_rmax", "year")

factor_cols <- c("regionNORTHEAST", "regionSOUTH", "regionWEST", "year", "zip")
data[factor_cols] <- lapply(data[factor_cols], factor)


# Add id to the data
data$id <- 1:nrow(data)

# subset of data based on region
data_south <- data[data$regionSOUTH == 1, ]
data_ne <- data[data$regionNORTHEAST == 1, ]
data_west <- data[data$regionWEST == 1, ]

set.seed(192)
random_subset <- sample(1:nrow(data_ne), 5000, replace = FALSE)

# Select subset of data
data_rn <- data_ne[random_subset, ]

set.seed(272)
ps_pop_obj_1 <- generate_pseudo_pop(data_rn[, c("id", "education")],
                                    data_rn[, c("id", "pm25")],
                                    data_rn[, c("id", confounders_1)],
                                    ci_appr = ci_appr,
                                    gps_model = gps_model,
                                    bin_seq = NULL,
                                    exposure_trim_qlts = c(0.01,
                                                           0.99),
                                    use_cov_transform = TRUE,
                                    params = list(xgb_max_depth = c(3,4,5),
                                                  xgb_nrounds = seq(10, 50, 1)),
                                    sl_lib = c("m_xgboost"),
                                    nthread = 4,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "maximal",
                                    max_attempt = 10,
                                    dist_measure = "l1",
                                    delta_n = 0.1,
                                    scale = 0.5)

q1 <- stats::quantile(data_rn$pm25,0.01)
q2 <- stats::quantile(data_rn$pm25,0.99)

pdf(paste0(sc_name, "-",
           ci_appr, "-",
           gps_model, "-",
           "cov-balance.pdf"))
plot(ps_pop_obj_1)
dev.off()

pseudo_pop <- ps_pop_obj_1
erf_obj <- estimate_npmetric_erf(pseudo_pop$pseudo_pop$education,
                                 pseudo_pop$pseudo_pop$pm25,
                                 pseudo_pop$pseudo_pop$counter_weight,
                                 bw_seq=seq(0.2, 2, 0.2),
                                 w_vals = seq(q1, q2, 0.2),
                                 nthread = 4,
                                 kernel_appr = "locpol")

pdf(paste0(sc_name, "-",
           ci_appr, "-",
           gps_model, "-",
           "erf-non-parametric.pdf"))
plot(erf_obj)
dev.off()
