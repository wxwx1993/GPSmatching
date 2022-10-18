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
table <- data.frame(matrix(c(
MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)

table <- data.frame(matrix(c(
  MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)

table <- data.frame(matrix(c(
  MSE_AB(data.list = dose_match.quad0.list, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta2, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_delta01, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale0, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale05, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_scale1, true.response = true.quad.response)[[2]],
  MSE_AB(data.list = dose_match.quad0.list_l2, true.response = true.quad.response)[[2]]), nrow=1))

print(xtable(table,digits=c(rep(2,15)),header=F), include.rownames=FALSE)
