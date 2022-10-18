# Generate Figures for the main text
library(nnet)
library("MASS")
library("SuperLearner")
library("xgboost")
library("earth")
library("ranger")
library("gam")
library("parallel")


MSMD_matched_CV <- function(replicate, 
                            scale, 
                            spec=1,
                            cova_spec=1,
                            sample_size=1000,
                            delta_n,
                            sl.lib){
  msmd_matched <- matrix(NA, nrow=replicate, ncol = 6)
  msmd_original <- matrix(NA, nrow=replicate, ncol = 6)
  msmd_matched.list <- list()
  msmd_original.list <- list()
 
  for (seed_id in 1:replicate){
    simulated_data <- data_generate(sample_size=sample_size,seed=seed_id,gps_spec= spec, cova_spec = cova_spec)
    c <- simulated_data[,-c(1:2)]
    treat <- as.vector(simulated_data[,"treat"])
    treat_quantile = quantile(treat,c(0.05,0.95))
    Y <- as.vector(simulated_data[,"Y"])
    match_data = run_sim_dose_matching(Y=Y,
                                       treat=treat,
                                       c=c,
                                       sl.lib=sl.lib,
                                       delta_n = delta_n,
                                       scale = scale)
    
    match_data<-subset(match_data, treat>= treat_quantile[1] & treat <= treat_quantile[2])
    match_data<-match_data[complete.cases(match_data),]
    
    simulated_data <- subset(simulated_data, treat>= treat_quantile[1] & treat <= treat_quantile[2])
    
    quintile <- quantile(simulated_data$treat, c(0,0.2,0.4,0.6,0.8,1))
    
    smd_matched <- matrix(NA, nrow = length(quintile)-1, ncol = 6)
    smd_original <- matrix(NA, nrow = length(quintile)-1, ncol = 6)
    for (i in 1:5) {
      match_data$treat_bin <- 0
      match_data$treat_bin[match_data$treat >= quintile[i] & match_data$treat < quintile[i+1]] <- 1
      
      smd_matched_cf5 = sapply((1:length(table(subset(match_data, treat_bin == 1)$cf5))), 
                                function(i){
                                  (table(subset(match_data, treat_bin ==1)$cf5)[i]/nrow(subset(match_data, treat_bin == 1))-
                                     table(subset(match_data, treat_bin ==0)$cf5)[i]/nrow(subset(match_data, treat_bin == 0)))/
                                    sqrt(table(match_data$cf5)[i]/nrow(match_data)*(1-table(match_data$cf5)[i]/nrow(match_data)))
                                }
      )
      
      smd_matched[i, ] <-
        c(abs(mean(subset(match_data, treat_bin ==1)$cf1)-mean(subset(match_data, treat_bin ==0)$cf1))/sd(match_data$cf1),
          abs(mean(subset(match_data, treat_bin ==1)$cf2)-mean(subset(match_data, treat_bin ==0)$cf2))/sd(match_data$cf2),
          abs(mean(subset(match_data, treat_bin ==1)$cf3)-mean(subset(match_data, treat_bin ==0)$cf3))/sd(match_data$cf3),
          abs(mean(subset(match_data, treat_bin ==1)$cf4)-mean(subset(match_data, treat_bin ==0)$cf4))/sd(match_data$cf4),
          mean(abs(smd_matched_cf5)),
          abs(mean(subset(match_data, treat_bin ==1)$cf6)-mean(subset(match_data, treat_bin ==0)$cf6))/sd(match_data$cf6))
                    
      
      simulated_data$treat_bin <- 0
      simulated_data$treat_bin[simulated_data$treat >= quintile[i] & simulated_data$treat < quintile[i+1]] <- 1
      
      smd_origin_cf5 = sapply((1:length(table(subset(simulated_data, treat_bin == 1)$cf5))), 
                               function(i){
                                 (table(subset(simulated_data, treat_bin ==1)$cf5)[i]/nrow(subset(simulated_data, treat_bin == 1))-
                                    table(subset(simulated_data, treat_bin ==0)$cf5)[i]/nrow(subset(simulated_data, treat_bin == 0)))/
                                   sqrt(table(simulated_data$cf5)[i]/nrow(simulated_data)*(1-table(simulated_data$cf5)[i]/nrow(simulated_data)))
                               }
      )
      
      smd_original[i, ] <-
        c(abs(mean(subset(simulated_data, treat_bin ==1)$cf1)-mean(subset(simulated_data, treat_bin ==0)$cf1))/sd(simulated_data$cf1),
          abs(mean(subset(simulated_data, treat_bin ==1)$cf2)-mean(subset(simulated_data, treat_bin ==0)$cf2))/sd(simulated_data$cf2),
          abs(mean(subset(simulated_data, treat_bin ==1)$cf3)-mean(subset(simulated_data, treat_bin ==0)$cf3))/sd(simulated_data$cf3),
          abs(mean(subset(simulated_data, treat_bin ==1)$cf4)-mean(subset(simulated_data, treat_bin ==0)$cf4))/sd(simulated_data$cf4),
          mean(abs(smd_origin_cf5)),
          abs(mean(subset(simulated_data, treat_bin ==1)$cf6)-mean(subset(simulated_data, treat_bin ==0)$cf6))/sd(simulated_data$cf6))
      
    }
    
    msmd_matched[seed_id, ] <- colMeans(smd_matched)
    msmd_original[seed_id, ] <- colMeans(smd_original)
    
    msmd_matched.list[[seed_id]] <- smd_matched
    msmd_original.list[[seed_id]] <- smd_original
   
  
  }
  return(list(c(colMeans(msmd_matched),colMeans(msmd_original)),
              Reduce("+", msmd_matched.list)/length(msmd_matched.list),
              Reduce("+", msmd_original.list)/length(msmd_original.list)
         ))
}
