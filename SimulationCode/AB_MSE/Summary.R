# Table 1: Absolute Bias and Root Mean Squared Error (RMSE). and Table S.4, 6-7

library("parallel")
library("MASS")
library(xtable)

MSE_AB<-function(data.list, true.response){
  
  sim.diff <- lapply(1:length(data.list), function(j){
    pred.response <- (data.list[[j]])
    return(pred.response - true.response)
  })
  
  DR.AB <- abs(rowMeans(apply(simplify2array(sim.diff), 1:2, mean)))
  DR.MSE <- sqrt(rowMeans(apply(simplify2array(sim.diff)^2, 1:2, mean)))
  
  return(list(rbind(DR.AB,DR.MSE),c(mean(DR.AB, na.rm = T),mean(DR.MSE, na.rm = T))))
}

## Table 1 and Table S.4
## Note DML was implemented separately by Python code obtained from Dr. Ying-ying Lee
############# matching
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size200gps_spec7cova_spec1.RData")

load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size1000gps_spec7cova_spec1.RData")


load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/Resubmission/Odessey/Result2/quad_size5000gps_spec7cova_spec1.RData")


data.name <- list.files("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch200", full.names = FALSE)
spec = 1
load(paste0("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch200/",data.name[spec]))
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]]


data.name <- list.files("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch1000", full.names = FALSE)
spec = 1
load(paste0("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch1000/",data.name[spec]))
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]]


data.name <- list.files("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch5000", full.names = FALSE)
spec = 1
load(paste0("~/Dropbox/continuous GPS/Resubmission/Odessey/Result1/optmatch5000/",data.name[spec]))
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]]

dose_match.quad0.list <- lapply(dose_match.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_match.quad0.list<- dose_match.quad0.list[-which(sapply(dose_match.quad0.list, is.null))]

dose_cova.quad0.list <- lapply(dose_cova.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_cova.quad0.list<- dose_cova.quad0.list[-which(sapply(dose_cova.quad0.list, is.null))]

dose_DR.quad0.list <- lapply(dose_DR.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_DR.quad0.list<- dose_DR.quad0.list[-which(sapply(dose_DR.quad0.list, is.null))]

dose_DR_trim.quad0.list <- lapply(dose_DR_trim.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_DR_trim.quad0.list<- dose_DR_trim.quad0.list[-which(sapply(dose_DR_trim.quad0.list, is.null))]


dose_IPTW.quad0.list <- lapply(dose_IPTW.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_IPTW.quad0.list<- dose_IPTW.quad0.list[-which(sapply(dose_IPTW.quad0.list, is.null))]

dose_IPTW_trim.quad0.list <- lapply(dose_IPTW_trim.quad0.list, function(x){
  if(is.na(tryCatch({sum(x)},error=function(i){
    return(NA)}))){return(NULL)}else{x}})
dose_IPTW_trim.quad0.list<- dose_IPTW_trim.quad0.list[-which(sapply(dose_IPTW_trim.quad0.list, is.null))]

############# CBPS
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size200gps_spec7cova_spec1.RData")

load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size1000gps_spec7cova_spec1.RData")


load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec6cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/CBPS/quad_cbps_size5000gps_spec7cova_spec1.RData")

MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_match_SL.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_cova.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_IPTW.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_DR.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_IPTW_trim.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_DR_trim.quad0.list, true.response = true.quad.response)[[2]]
MSE_AB(data.list = dose_CBPS.quad0.list, true.response = true.quad.response)[[2]]


table <- data.frame(matrix(c(
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_cova.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_IPTW.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_DR.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_IPTW_trim.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_DR_trim.quad0.list, true.response = true.quad.response)[[2]]),nrow=1))

print(xtable(table,digits=c(rep(2,13)),header=F), include.rownames=FALSE)


## Table S.6-7: Matching sensitivity
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching200gps_spec6cova_spec1.RData")

table <- data.frame(matrix(c(
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)


load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching1000gps_spec6cova_spec1.RData")


table <- data.frame(matrix(c(
  MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)

load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec1cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec2cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec3cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec4cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec5cova_spec1.RData")
load("~/Dropbox/continuous GPS/JASA resubmission/Simulation/Sensitivity/sens_matching5000gps_spec6cova_spec1.RData")

table <- data.frame(matrix(c(
  MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)

