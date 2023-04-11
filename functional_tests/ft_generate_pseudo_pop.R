set.seed(422)
n <- 4000
mydata <- generate_syn_data(sample_size=n)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
set_logger(logger_level = "DEBUG")


id <- seq_along(1:nrow(mydata))

Y <- data.frame(id = id, Y = mydata$Y)
w <- data.frame(id = id, w = mydata$w)
c <- data.frame(id = id, mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")])

pseudo_pop <- generate_pseudo_pop(Y,
                                  w,
                                  c,
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
                                  max_attempt = 10,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 12)

plot(pseudo_pop)
