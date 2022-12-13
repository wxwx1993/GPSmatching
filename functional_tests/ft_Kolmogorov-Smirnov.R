library("Ecume")
library("CausalGPS")
set.seed(422)
n <- 4000
mydata <- generate_syn_data(sample_size=n)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
set_logger(logger_level = "DEBUG")

pseudo_pop <- generate_pseudo_pop(mydata$Y,
                                  mydata$treat,
                                  mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
                                  ci_appr = "matching",
                                  gps_model = "non-parametric",
                                  use_cov_transform = TRUE,
                                  transformers = list("pow2", "pow3", "abs", "scale"),
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
                                  nthread = 12)

plot(pseudo_pop)

tmp_data <- pseudo_pop$pseudo_pop
data.table::setDF(tmp_data)
counter_weight <- tmp_data$counter_weight
tmp_data <- tmp_data[, c("cf1","cf2","cf3","cf4","cf5","cf6","year","region","w")]

ks_stat <- lapply(1:(ncol(tmp_data)), function(i) {ks_test(as.numeric(tmp_data[,i]),
                                                           as.numeric(tmp_data[,i]),
                                                           w_x = rep(1, nrow(tmp_data)),
                                                           w_y = counter_weight)$statistic})

ks_stat <- unlist(ks_stat)
