

set.seed(721)
s_data <- generate_syn_data(sample_size=200,
                            outcome_sd = 10, gps_spec = 1,
                            cova_spec = 1)

s_data$id <- seq_along(1:nrow(s_data))
s_data$w <- s_data$treat
s_data$treat <- NULL

data_with_gps_test <- estimate_gps(s_data[, c("id", "w")],
                                   s_data[, c("id", "cf1", "cf2", "cf3",
                                              "cf4","cf5", "cf6")],
                                   internal_use = TRUE,
                                   sl_lib = c("SL.xgboost","SL.earth",
                                              "SL.gam","SL.ranger")
)
