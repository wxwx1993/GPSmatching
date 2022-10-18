process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library("parallel")
source("GPSmatching.R")
source("data_generate.R")
source("Alternatives.R")

sim_num = 500
spec = process + 1
cova_spec = 1
sample_size = 1000

simulated_data <- data_generate(sample_size=sample_size,seed=1, gps_spec= spec, cova_spec = cova_spec)
treat <- as.vector(simulated_data[,"treat"])
a.vals<-sort(sample(subset(treat, treat <= quantile(treat,0.95) & treat >= quantile(treat,0.05)), 500))
#true.quad.response<-(-0+1*a.vals+0.05*a.vals^2) + 0.001*a.vals^3
true.quad.response<-(-(1+0.2*(a.vals-20)-0.13^2*(a.vals-20)^3))
#############matching

dose_match.quad0.list<-mclapply(1:sim_num,
                               function(seed_id,a.vals){
                                  simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                                  c <- simulated_data[,-c(1:2)]
                                  treat <- as.vector(simulated_data[,"treat"])
                                  Y <- as.vector(simulated_data[,"Y"])
                                  match_data = run_sim_dose_matching(Y=Y,
                                                                     treat=treat,
                                                                     c=c,
                                                                     sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                                                     delta_n = 0.5,
                                                                     scale = 1)
                                  return(matching_smooth(pseudo.out=match_data$Y,
                                                         bw.seq= seq(0.5,5,0.2),
                                                         a.vals= a.vals,
                                                         a=match_data$treat))
                                },
                                mc.cores=16,a.vals=a.vals)


dose_DR.quad0.list<-mclapply(1:sim_num,
                             function(seed_id,a.vals){
                               simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                               c <- simulated_data[,-c(1:2)]
                               treat <- as.vector(simulated_data[,"treat"])
                               Y <- as.vector(simulated_data[,"Y"])
                               return(ctseff(y=Y,
                                             a=treat,
                                             x=c, 
                                             bw.seq=seq(0.5,5,0.5), 
                                             a.vals=a.vals,
                                             sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger")))
                               
                             },
                             mc.cores=16,a.vals=a.vals)


dose_cova.quad0.list<-mclapply(1:sim_num,
                                function(seed_id,a.vals){
                                  simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                                  c <- simulated_data[,-c(1:2)]
                                  treat <- as.vector(simulated_data[,"treat"])
                                  Y <- as.vector(simulated_data[,"Y"])
                                  return(run_sim_dose_cova(Y=Y,
                                                           treat=treat,
                                                           c=c,
                                                           sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                                           a.vals = a.vals))
                                },
                                mc.cores=16,a.vals=a.vals)

dose_IPTW.quad0.list<-mclapply(1:sim_num,
                                function(seed_id,a.vals){
                                  simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                                  c <- simulated_data[,-c(1:2)]
                                  treat <- as.vector(simulated_data[,"treat"])
                                  Y <- as.vector(simulated_data[,"Y"])
                                  return(run_sim_dose_IPTW(Y=Y,
                                                           treat=treat,
                                                           c=c,
                                                           sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                                           a.vals = a.vals))
                                  
                                },
                                mc.cores=16,a.vals=a.vals)

dose_DR_trim.quad0.list<-mclapply(1:sim_num,
                             function(seed_id,a.vals){
                               simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                               c <- simulated_data[,-c(1:2)]
                               treat <- as.vector(simulated_data[,"treat"])
                               Y <- as.vector(simulated_data[,"Y"])
                               return(ctseff_trim(y=Y,
                                             a=treat,
                                             x=c, 
                                             bw.seq=seq(0.5,5,0.5), 
                                             a.vals=a.vals,
                                             sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger")))
                               
                             },
                             mc.cores=16,a.vals=a.vals)


dose_IPTW_trim.quad0.list<-mclapply(1:sim_num,
                                function(seed_id,a.vals){
                                  simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
                                  c <- simulated_data[,-c(1:2)]
                                  treat <- as.vector(simulated_data[,"treat"])
                                  Y <- as.vector(simulated_data[,"Y"])
                                  return(run_sim_dose_IPTW_trim(Y=Y,
                                                           treat=treat,
                                                           c=c,
                                                           sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                                           a.vals = a.vals))
                                  
                                },
                                mc.cores=16,a.vals=a.vals)



save(a.vals,true.quad.response,dose_match.quad0.list,dose_cova.quad0.list,dose_IPTW.quad0.list,dose_DR.quad0.list,dose_IPTW_trim.quad0.list,dose_DR_trim.quad0.list,
     file=paste0("/n/home01/xwu1993/GPSmatching/results/quad_size",sample_size,"gps_spec",spec,"cova_spec",cova_spec,".RData"))      
