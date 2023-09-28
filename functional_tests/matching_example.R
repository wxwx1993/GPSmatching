##      author:  Naeem Khoshnevis
##      created: September 2023
##      purpose: Reproducing examples in the paper.


# Load libraries ---------------------------------------------------------------

library(ggplot2)
library(CausalGPS)
library(memoise)
library(data.table)

# Load data --------------------------------------------------------------------

data_file <- "zip_data.RData"
if (!file.exists(data_file)) {
  stop(paste0("Download the study data file from the following link:\n",
             "https://drive.google.com/file/d/",
             "1QFdbVU8Qir1gWf96c5h_ZhT-aPjhHpqn/view?usp=share_link"))
} else {
  load(local_path)
}

data.table::setDF(zip_data)
data <- zip_data

# Select Example ---------------------------------------------------------------

## Available options:
# matching_1 (first setting)
#  - exposure trim %1-%99 value
#  - delta_n = 1.7

# matching_2 (second setting)
#  - exposure trim %5-%95 value
#  - delta_n = 0.9

# matching_3 (third setting)
#  - exposure trim %10-%90 value
#  - delta_n = 1.9


example <- "matching_3"

#------To reproduce the same results, do not change below this line.------------

if (example == "matching_1") {
  p_q1 <- 0.01
  p_q2 <- 0.99
  delta_n <- 1.7
} else if (example == "matching_2"){
  p_q1 <- 0.05
  p_q2 <- 0.95
  delta_n <- 0.9
} else if (example == "matching_3"){
  p_q1 <- 0.10
  p_q2 <- 0.90
  delta_n <- 1.9
} else {
  stop("Example type is not defined.")
}

sc_name <- example
ci_appr <- "matching"
gps_density <- "normal"

max_it <- 10
scale_val <- 1

# Select confounders -----------------------------------------------------------
confounders   <- c("mean_bmi", "smoke_rate",
                   "hispanic", "pct_blk", "medhouseholdincome",
                   "medianhousevalue", "poverty", "popdensity",
                   "pct_owner_occ", "summer_tmmx", "winter_tmmx",
                   "summer_rmax", "winter_rmax", "year")

factor_cols <- c("year", "zip")
data[factor_cols] <- lapply(data[factor_cols], factor)

# Add id to the data
data$id <- 1:nrow(data)

seed_val <- 249
file_name <- paste0(sc_name, "-",
                    ci_appr, "-",
                    gps_density, "-",
                    "q-", p_q1, "-", p_q2, "-",
                    "seed-", seed_val, "-",
                    "maxit-", max_it, "-",
                    "delta-", delta_n, "-",
                    "sc-", scale_val,
                    "-maximal")

set_logger(logger_file_path = paste0("log-", file_name, ".txt"),
           logger_level = "TRACE")

start_time <- proc.time()

set.seed(seed_val)
ps_pop_obj_1 <- generate_pseudo_pop(data[, c("id", "pm25")],
                                    data[, c("id", confounders)],
                                    ci_appr = ci_appr,
                                    gps_density = gps_density,
                                    bin_seq = NULL,
                                    exposure_trim_qtls = c(p_q1,
                                                           p_q2),
                                    use_cov_transform = TRUE,
                                    params = list(xgb_max_depth = c(3,4,5),
                                                  xgb_nrounds = seq(10, 40, 1)),
                                    sl_lib = c("m_xgboost"),
                                    nthread = 10,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type= "maximal",
                                    max_attempt = max_it,
                                    dist_measure = "l1",
                                    delta_n = delta_n,
                                    scale = scale_val)

end_time <- proc.time()
write(paste0(file_name,
             " | Wall clock time:  ",
             (end_time - start_time)[[3]]," seconds."),
      "wc_times.txt", append = TRUE)

pdf(paste0(file_name, "-",
           "cov-balance.pdf"))
plot(ps_pop_obj_1, include_details = TRUE)
dev.off()

saveRDS(ps_pop_obj_1, file = paste0(file_name,"-object.rds"))


