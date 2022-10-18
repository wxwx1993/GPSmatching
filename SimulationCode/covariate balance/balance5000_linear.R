process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library("parallel")
source("Covariate_balance.R")
source("data_generate.R")
source("GPSmatching.R")

parameters <- expand.grid(seq(0.1,0.4,0.1),c(8))[(process+1),]
sim_num = 500
spec = as.numeric(parameters[2])
cova_spec = 1
sample_size = 5000
delta_n = as.numeric(parameters[1])

MAC_matched<-mclapply(replicate=sim_num,seq(0.1,1,0.1),MAC_matched_CV,spec=spec,sample_size=sample_size,delta_n=delta_n,sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),mc.cores=16)

MAC_matched.df<-data.frame(matrix(unlist(MAC_matched), nrow=10, byrow=T))


save(MAC_matched.df,
  file=paste0("/n/home01/xwu1993/GPSmatching/balance/MAC_match",sample_size,"gps_spec",spec,"cova_spec",cova_spec,"delta_n",delta_n,".RData"))

