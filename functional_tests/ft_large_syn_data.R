set.seed(422)
n <- 10000
mydata <- generate_syn_data(sample_size=n, vectorized_y = TRUE)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
set_logger(logger_level = "TRACE")


id <- seq_along(1:nrow(mydata))

Y <- data.frame(id = id, out = mydata$Y)
w <- data.frame(id = id, treatment = mydata$w)
c <- data.frame(id = id, mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")])

ps_pop1 <- generate_pseudo_pop(Y,
                               w,
                               c,
                               ci_appr = "matching",
                               gps_density = "kernel",
                               exposure_trim_qtls = c(0.01,0.99),
                               gps_trim_qtls = c(0,1),
                               sl_lib = c("m_xgboost"),
                               covar_bl_method = "absolute",
                               covar_bl_trs = 0.1,
                               covar_bl_trs_type = "mean",
                               params = list(xgb_max_depth = c(3,4,5),
                                             xgb_nrounds=seq(10,50,1)),
                               max_attempt = 10,
                               dist_measure = "l1",
                               delta_n = 1,
                               scale = 0.5,
                               nthread = 10)

plot(ps_pop1)

set.seed(168)
erf_pmtric <- estimate_pmetric_erf(formula = out ~ treatment,
                                   family = gaussian,
                                   data = ps_pop1$pseudo_pop)


set.seed(168)
erf_semipmetric <- estimate_semipmetric_erf(formula = out ~ treatment,
                                            family = gaussian,
                                            data = ps_pop1$pseudo_pop)


set.seed(168)
erf_npmetric <- estimate_npmetric_erf(m_Y = ps_pop1$pseudo_pop$out,
                                      m_w = ps_pop1$pseudo_pop$treatment,
                                      counter_weight = ps_pop1$pseudo_pop$counter_weight,
                                      bw_seq = seq(0.2,10,0.05),
                                      w_vals = seq(7, 13, 0.5),
                                      nthread = 10)


