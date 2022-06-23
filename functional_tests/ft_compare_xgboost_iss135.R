rm(list = ls())
library(tidyverse)
library(caret)
library(data.table)
library(weights)
library("devtools")
library(ggplot2)
#install_github("fasrc/CausalGPS")
#library("CausalGPS")

# Example script of CausalGPS not finding covariate balance
n <- 1000
set.seed(23)
# Simulate covariates, exposure (w) and outcome (y) as interaction of all three
cov <- data.frame(c1 = runif(n), c2 = rnorm(n), c3 = rbinom(n, 1, 0.5))
w <- rnorm(n, cov$c1 * cov$c2 * cov$c3)
y <- 0.1 * w + cov$c1 + cov$c2 * cov$c3 + rnorm(n)

# Train xgboost model using caret package
tune_control <-
  caret::trainControl(method = "cv",
                      number = 5,
                      verboseIter = FALSE,
                      allowParallel = TRUE)
nrounds <- 400
tune_grid <- expand.grid(nrounds = seq(from = 50, to = nrounds, by = 50),
                         eta = c(0.025, 0.05, 0.1),
                         max_depth = c(2),
                         gamma = 0,
                         colsample_bytree = 1,
                         min_child_weight = c(1,2),
                         subsample = 1)
xgbt_model <-
  train(w ~ ., data = data.frame(cbind(w, cov)), method = "xgbTree",
        trControl = tune_control, tuneGrid = tune_grid, verbose = 0)

# Get predictions from model and compare
predVals <- extractPrediction(list(xgbt_model))
#plotObsVsPred(predVals)

ipw2 <- dnorm(w, mean(w), sd(w)) / dnorm(w, predVals$pred, sqrt(mean(resid(xgbt_model)^2)))

correlation_table <-
  rbindlist(lapply(names(data.frame(cov)), function(c){
    pre_cor <- abs(cor(w, cov[[c]]))
    post_cor <- abs(wtd.cor(w, cov[[c]], ipw2))
    data.table(covariate = c, pre_cor = pre_cor, post_cor = post_cor[1])
  }))

# Now extract best tune values for gps prediction function
data_with_gps <-
  estimate_gps(y,
               w,
               cov,
               gps_model = "parametric",
               pred_model = "sl",
               internal_use = FALSE,
               params = list(xgb_max_depth = c(xgbt_model$bestTune$max_depth),
                             xgb_rounds = c(xgbt_model$bestTune$nrounds),
                             xgb_eta = c(xgbt_model$bestTune$eta)),
               nthread = 1,
               sl_lib = c("m_xgboost"))

# Compare the correlations here, notice there is no covariate balance achieved
correlation_table_causal <-
  rbindlist(lapply(c("c1", "c2", "c3"), function(c){
    pre_cor <- abs(cor(data_with_gps[[1]]$w, data_with_gps[[1]][, c]))
    post_cor <- abs(wtd.cor(data_with_gps[[1]]$w, data_with_gps[[1]][, c], data_with_gps[[1]]$gps))
    data.table(covariate = c, pre_cor = pre_cor, post_cor = post_cor[1])
  }))

correlation_table
correlation_table_causal

# The GPS values are different.
g <- ggplot() + geom_density(aes(ipw2), color="blue")
g <- g + geom_density(aes(data_with_gps$dataset$gps), color="red")
plot(g)


# Generating pseudo population
set.seed(23)
pseudo_pop <- generate_pseudo_pop(y,
                                  w,
                                  cov,
                                  ci_appr = "matching",
                                  pred_model = "sl",
                                  gps_model = "non-parametric",
                                  use_cov_transform = TRUE,
                                  transformers = list("pow2", "pow3", "abs", "scale"),
                                  trim_quantiles = c(0.00,1.0),
                                  optimized_compile = TRUE,
                                  sl_lib = c("m_xgboost"),
                                  params = list(xgb_max_depth = c(xgbt_model$bestTune$max_depth),
                                                xgb_rounds = c(xgbt_model$bestTune$nrounds),
                                                xgb_eta = c(xgbt_model$bestTune$eta),
                                                xgb_min_child_weight = c(xgbt_model$bestTune$min_child_weight)
                                                ),
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type = "mean",
                                  max_attempt = 4,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5,
                                  nthread = 1)

plot(pseudo_pop)
