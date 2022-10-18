process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

library("parallel")
source("CBPS_balance.R")
source("data_generate.R")

sim_num = 500
spec = process + 1
cova_spec = 1
sample_size = 5000

MAC_cbps<-MAC_cbps_CV(replicate=sim_num,spec=spec,sample_size=sample_size,cova_spec=cova_spec)

MAC_cbps.df<-data.frame(matrix(unlist(MAC_cbps), nrow=1, byrow=T))


save(MAC_cbps.df,
  file=paste0("/n/home01/xwu1993/GPSmatching/balance/MAC_cbps",sample_size,"gps_spec",spec,"cova_spec",cova_spec,".RData"))

