#MAC_matched<-matrix(NA,nrow=20,ncol=6)
process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library("parallel")
source("Covariate_balance_MSMD.R")
source("data_generate.R")
source("GPSmatching.R")

parameters <- expand.grid(seq(0.1,0.4,0.1),1:7)[(process+1),]
sim_num = 100
spec = as.numeric(parameters[2])
cova_spec = 1
sample_size = 5000
delta_n = as.numeric(parameters[1])

MSMD_matched<-mclapply(replicate=sim_num,seq(0.1,1,0.1),MSMD_matched_CV,spec=spec,sample_size=sample_size,delta_n=delta_n,sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),mc.cores=16)

MSMD_matched.df<-data.frame(matrix(unlist(MSMD_matched), nrow=10, byrow=T))


save(MSMD_matched, MSMD_matched.df,
  file=paste0("/n/home01/xwu1993/GPSmatching/balance/MSMD",sample_size,"gps_spec",spec,"cova_spec",cova_spec,"delta_n",delta_n,".RData"))

