process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library("parallel")
library("CausalGPS")
source("GPSmatching.R")
source("data_generate.R")

parameters <- expand.grid(1:500,c(1:7))[(process+1),]

spec = as.numeric(parameters[2])
cova_spec = 1
sample_size = 1000
delta_n <- c(0.4, 1.4, 0.5, 0.6, 0.4, 0.4, 1.4, 0.4)[as.numeric(parameters[2])]
scale <- c(0.4, 1.0, 1.0, 1.0, 0.7, 0.2, 1.0, 1.0)[as.numeric(parameters[2])]

simulated_data <- data_generate(sample_size = sample_size, seed = 1, gps_spec = spec, cova_spec = cova_spec)
treat <- as.vector(simulated_data[,"treat"])
a.vals<-sort(sample(subset(treat, treat <= quantile(treat,0.95) & treat >= quantile(treat,0.05)), 100))
true.quad.response<-(-(1+0.2*(a.vals-20)-0.13^2*(a.vals-20)^3))
#############matching

simulated_data <- data_generate(sample_size = sample_size, seed = as.integer(parameters[1] + 1), gps_spec = spec, cova_spec = cova_spec)

match_data <- generate_pseudo_pop(simulated_data$Y,
                                  simulated_data$treat,
                                  simulated_data[,3:8],
                                  ci_appr = "matching",
                                  running_appr = "base",
                                  pred_model = "sl",
                                  gps_model = "non-parametric",
                                  use_cov_transform = FALSE,
                                  sl_lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                  nthread = 1, # number of cores, you can change,
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  trim_quantiles = c(0.05,0.95), # trimed, you can change,
                                  optimized_compile = FALSE, #created a column counter for how many times matched,
                                  max_attempt = 1,
                                  matching_fun = "matching_l1",
                                  delta_n = delta_n,
                                  scale = scale)
erf <- estimate_npmetric_erf(matched_Y = match_data$pseudo_pop$Y,
                             matched_w = match_data$pseudo_pop$w,
                             bw_seq = seq(0.2,2,0.2),
                             w_vals = a.vals,
                             nthread = 1)

## Bootstrap
  match_coefs_boots <- sapply(1:500, function(boots_id){
  set.seed(boots_id)
  subset <- simulated_data[sample(1:sample_size,floor(2*sqrt(sample_size)),replace=T), ]
  match_data <- generate_pseudo_pop(subset$Y,
                                    subset$treat,
                                    subset[,3:8],
                                    ci_appr = "matching",
                                    running_appr = "base",
                                    pred_model = "sl",
                                    gps_model = "non-parametric",
                                    use_cov_transform = FALSE,
                                    sl_lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                    nthread = 1, # number of cores, you can change,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    trim_quantiles = c(0.05,0.95), # trimed, you can change,
                                    optimized_compile = FALSE, #created a column counter for how many times matched,
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = delta_n,
                                    scale = scale)
  erf_boots <- estimate_npmetric_erf(matched_Y = match_data$pseudo_pop$Y,
                                     matched_w = match_data$pseudo_pop$w,
                                     bw_seq = seq(0.2,2,0.2),
                                     w_vals = a.vals,
                                     nthread = 1) 
  return(erf_boots$erf)
  })
      

ci_coverage <- sapply(1:100, function(i) {
  
  wald_ci <- c(erf$erf[i] - 1.96*sd(match_coefs_boots[i,], na.rm = T) *sqrt(2*sqrt(sample_size))/sqrt(sample_size),
        erf$erf[i] + 1.96*sd(match_coefs_boots[i,], na.rm = T) *sqrt(2*sqrt(sample_size))/sqrt(sample_size))
  coverage <- c(wald_ci[1] < true.quad.response[i]) & (wald_ci[2] > true.quad.response[i])
  
  return(c(wald_ci, coverage))
 })

saveRDS(ci_coverage,
     file=paste0("/n/home01/xwu1993/GPSmatching2/coverage/1000/coverage",sample_size,"gps_spec",spec,"cova_spec",cova_spec,"process",process, ".rds"))



