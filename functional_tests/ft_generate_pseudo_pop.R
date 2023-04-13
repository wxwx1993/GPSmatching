set.seed(422)
n <- 2000
mydata <- generate_syn_data(sample_size=n)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
set_logger(logger_level = "INFO")


id <- seq_along(1:nrow(mydata))

Y <- data.frame(id = id, out = mydata$Y)
w <- data.frame(id = id, treatment = mydata$w)
c <- data.frame(id = id, mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")])

pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
                                  ci_appr = "matching",
                                  gps_density = "kernel",
                                  use_cov_transform = TRUE,
                                  transformers = list("pow2", "pow3",
                                                      "abs", "scale"),
                                  trim_quantiles = c(0.01,0.99),
                                  optimized_compile = TRUE,
                                  sl_lib = c("m_xgboost"),
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type = "mean",
                                  max_attempt = 4,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 12)

plot(pseudo_pop)


gps_obj <-


gps_obj <- estimate_gps(w,
                        c,
                        gps_density = "normal",
                        params = list(xgb_max_depth = c(3,4,5),
                                      xgb_nrounds=c(10,20,30,40,50,60)),
                        nthread = 1,
                        sl_lib = c("m_xgboost")
                        )

pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
                                  ci_appr = "matching",
                                  gps_obj = gps_obj,
                                  use_cov_transform = TRUE,
                                  trim_quantiles = c(0.01,0.99),
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type = "mean",
                                  matching_fun = "matching_l1",
                                  max_attempt = 1,
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 12)

plot(pseudo_pop)
