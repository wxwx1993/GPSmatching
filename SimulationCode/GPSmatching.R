##################GPS matching functions
require(parallel)
library(data.table)
require(KernSmooth)
library("SuperLearner")
library("xgboost")
library("earth")
library("ranger")
library("gam")

mc_cores = 1

gc()
###########Matching on single exposure level a
matching.fun.dose.l1.caliper2 <- function(dataset,
                                          e_gps_pred,
                                          e_gps_std_pred,
                                          a,
                                          c,
                                          delta_n=1,
                                          w_resid,
                                          scale)
{
  ## cosmetic changes only
  w_new <- (a-e_gps_pred)/e_gps_std_pred
  p.a <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_new,rule=2)$y
  ## calculate min and max once, cache result
  treat.min <- min(dataset[["treat"]],na.rm=T)
  treat.max <- max(dataset[["treat"]],na.rm=T)
  GPS.min <- min(dataset[["GPS"]],na.rm=T)
  GPS.max <- max(dataset[["GPS"]],na.rm=T)
  ## using transform instead of $ is mostly cosmetic
  dataset <- transform(dataset,
                              std.treat = (treat - treat.min) / (treat.max - treat.min),
                              std.GPS = (GPS - GPS.min) / (GPS.max - GPS.min))
  std.a <- (a - treat.min) / (treat.max - treat.min)
  std.p.a <- (p.a - GPS.min) / (GPS.max - GPS.min)
  ## this subsetting doesn't depend on i, and therefore doesn't need to be done on each iteration
  dataset.subset <- dataset[abs(dataset[["treat"]] - a) <= (delta_n/2), ]
  ## doing the subtraction with `outer` is faster than looping over with sapply or parSapply
  wm <- apply(abs(outer(dataset.subset[["std.GPS"]], std.p.a, `-`)) * scale,
              2,
              function(x) which.min(abs(dataset.subset[["std.treat"]] - std.a) * (1 - scale) + x)
  )
  dp <- dataset.subset[wm,]
  return(dp)
  gc()
}

########################
run_sim_dose_matching<-function(Y,
                                treat,
                                c,
                                matching_fun=matching.fun.dose.l1.caliper2,
                                sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                                scale=0.5,
                                delta_n=1){
  ##### GPS function
  e_gps <- SuperLearner(Y=treat, X=data.frame(c), SL.library=sl.lib)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- SuperLearner(Y=abs(treat-e_gps_pred), X=c, SL.library=sl.lib)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- (treat-e_gps_pred)/e_gps_std_pred
  GPS <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_resid,rule=2)$y
  
  dataset <- cbind(Y,treat,c,GPS)
  
  bin.num<-seq(min(dataset$treat)+delta_n/2, max(dataset$treat), by = delta_n)
  
  match_data <-  lapply(bin.num, matching_fun, 
                        dataset=dataset,
                        e_gps_pred = e_gps_pred,
                        e_gps_std_pred = e_gps_std_pred,
                        c,
                        delta_n=delta_n,
                        w_resid=w_resid,
                        scale=scale)
  
  return(data.table(Reduce(rbind,match_data)))
}

matching_smooth<-function(pseudo.out=dose.response.mean,
                          a,
                          bw.seq=seq(1,1,length.out=10),
                          a.vals){
  kern <- function(t){ dnorm(t) }
  w.fn <- function(bw){ w.avals <- NULL; for (a.val in a.vals){
    a.std <- (a-a.val)/bw; kern.std <- kern(a.std)/bw
    w.avals <- c(w.avals, mean(a.std^2*kern.std)*(kern(0)/bw) /
                   (mean(kern.std)*mean(a.std^2*kern.std)-mean(a.std*kern.std)^2))
  }; return(w.avals/length(a)) }
  hatvals <- function(bw){ approx(a.vals,w.fn(bw),xout=a,rule=2)$y }
  cts.eff.fn <- function(out,bw){
    approx(locpoly(a,out,bandwidth=bw, gridsize=1000),xout=a,rule=2)$y }
  # note: choice of bandwidth range depends on specific problem,
  # make sure to inspect plot of risk as function of bandwidth
  risk.fn <- function(h){ hats <- hatvals(h); mean( ((pseudo.out - cts.eff.fn(pseudo.out,bw=h))/(1-hats))^2) }
  risk.est <- sapply(bw.seq,risk.fn); 
  h.opt <- bw.seq[which.min(risk.est)]
  bw.risk <- data.frame(bw=bw.seq, risk=risk.est)
  
  est <- approx(locpoly(a,pseudo.out,bandwidth=h.opt),xout=a.vals)$y
  return(est)
}
