library(nnet)
library("MASS")
library("SuperLearner")
library("xgboost")
library("earth")
library("ranger")
library("gam")
require(KernSmooth)
library("CBPS")

##### Adjustement approach, including GPS as covariates
GPScova.fun.dose<-function(e_gps_pred,
                           e_gps_std_pred,
                           w_resid,
                           a,
                           model=flexible_model){
  #p.a<-dnorm(a,mean = predict(GPS_mod,simulated.data),sd=summary(GPS_mod)$sigma)
  w_new <- (a-e_gps_pred)/e_gps_std_pred
  p.a <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_new,rule=2)$y
  data.a<-data.frame(cbind(treat=a,GPS=p.a))
  E.a<-predict(model,data.a)[[1]]
  
  return(c(mean(E.a)))
}

run_sim_dose_cova<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  e_gps <- SuperLearner(Y=treat, X=data.frame(c), SL.library=sl.lib)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- SuperLearner(Y=abs(treat-e_gps_pred), X=c, SL.library=sl.lib)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- (treat-e_gps_pred)/e_gps_std_pred
  GPS <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_resid,rule=2)$y
  
  data_set<-data.frame(cbind(treat=treat,GPS=GPS))
  
  flexible_model<-SuperLearner(Y=Y, X=data_set, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  
  GPScova_data <- sapply(a.vals,
                         GPScova.fun.dose, 
                         e_gps_pred = e_gps_pred, e_gps_std_pred = e_gps_std_pred, w_resid=w_resid, model=flexible_model)
  return(GPScova_data)
}


############## IPTW approach, or call MSMs
IPW.fun.dose<-function(e_gps_pred,
                       e_gps_std_pred,
                       w_resid,
                       a,
                       model=IPW_model){
  #p.a<-dnorm(a,mean = predict(GPS_mod,simulated.data),sd=summary(GPS_mod)$sigma)
  w_new <- (a-e_gps_pred)/e_gps_std_pred
  p.a <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_new,rule=2)$y
  data.a<-data.frame(cbind(treat=a))
  E.a<-predict(model,data.a)[[1]]
  
  return(c(mean(E.a)))
}

run_sim_dose_IPTW<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  e_gps <- SuperLearner(Y=treat, X=data.frame(c), SL.library=sl.lib)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- SuperLearner(Y=abs(treat-e_gps_pred), X=c, SL.library=sl.lib)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- (treat-e_gps_pred)/e_gps_std_pred
  GPS <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_resid,rule=2)$y
  
  Nm <- approx(density(treat,na.rm = TRUE)$x,density(treat,na.rm = TRUE)$y,xout=treat,rule=2)$y
  IPW<-Nm/(GPS)
  
  IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), obsWeights = IPW, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  
  IPTW_data<- sapply(a.vals,
                     IPW.fun.dose,
                     e_gps_pred = e_gps_pred, 
                     e_gps_std_pred = e_gps_std_pred, 
                     w_resid=w_resid, 
                     model=IPTW_model)
  
  return(IPTW_data)
}

run_sim_dose_IPTW_trim<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  e_gps <- SuperLearner(Y=treat, X=data.frame(c), SL.library=sl.lib)
  e_gps_pred <- e_gps$SL.predict
  e_gps_std <- SuperLearner(Y=abs(treat-e_gps_pred), X=c, SL.library=sl.lib)
  e_gps_std_pred <- e_gps_std$SL.predict
  w_resid <- (treat-e_gps_pred)/e_gps_std_pred
  GPS <- approx(density(w_resid,na.rm = TRUE)$x,density(w_resid,na.rm = TRUE)$y,xout=w_resid,rule=2)$y
  
  Nm <- approx(density(treat,na.rm = TRUE)$x,density(treat,na.rm = TRUE)$y,xout=treat,rule=2)$y
  IPW<-Nm/(GPS)
  IPW[IPW >10] <- 10
  #if (sum(simulated.data$IPW>10)>0){simulated.data[which(simulated.data$IPW>10),]$IPW<-10}
  
  IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), obsWeights = IPW, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  #IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), SL.library=sl.lib)
  #IPTW_model<-lm(Y~treat+I(treat^2)+I(treat^3),weights=IPW)
  
  IPTW_data<- sapply(a.vals,
                     IPW.fun.dose,
                     e_gps_pred = e_gps_pred, 
                     e_gps_std_pred = e_gps_std_pred, 
                     w_resid=w_resid, 
                     model=IPTW_model)
  
  return(IPTW_data)
}


############## DR approach, the following code were modified from ehkennedy/npcausal R package
ctseff <- function(y,
                   a,
                   x, 
                   bw.seq, 
                   sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                   a.vals){
  
  kern <- function(t){ dnorm(t) }
  n <- dim(x)[1]
  
  # set up evaluation points & matrices for predictions
  a.min <- min(a.vals); a.max <- max(a.vals)
  xa.new <- rbind(cbind(x,a), cbind( x[rep(1:n,length(a.vals)),],a=rep(a.vals,rep(n,length(a.vals))) ))
  x.new <- xa.new[,-dim(xa.new)[2]]
  x <- data.frame(x); x.new <- data.frame(x.new); colnames(x) <- colnames(x.new)
  xa.new <- data.frame(xa.new)
  
  # estimate nuisance functions via super learner
  # note: other methods could be used here instead
  pimod <- SuperLearner(Y=a, X=data.frame(x), SL.library=sl.lib, newX=x.new)
  pimod.vals <- pimod$SL.predict
  pi2mod <- SuperLearner(Y=abs(a-pimod.vals[1:n]),X=x, SL.library=sl.lib, newX=x.new)
  pi2mod.vals <- pi2mod$SL.predict
  mumod <- SuperLearner(Y=y, X=cbind(x,a), SL.library=sl.lib,newX=xa.new)
  muhat.vals <- mumod$SL.predict
  
  # construct estimated pi/varpi and mu/m values
  a.std <- (xa.new$a-pimod.vals)/pi2mod.vals
  pihat.vals <- approx(density(a.std[1:n],na.rm = T)$x,density(a.std[1:n],na.rm = T)$y,xout=a.std,rule=2)$y
  pihat <- pihat.vals[1:n]; pihat.mat <- matrix(pihat.vals[-(1:n)], nrow=n,ncol=length(a.vals))
  varpihat <- predict(smooth.spline(a.vals, apply(pihat.mat,2,mean,na.rm=T)), x=a)$y
  varpihat.mat <- matrix( rep(apply(pihat.mat,2,mean,na.rm=T),n), byrow=T, nrow=n)
  muhat <- muhat.vals[1:n]; muhat.mat <- matrix(muhat.vals[-(1:n)], nrow=n,ncol=length(a.vals))
  mhat <- predict(smooth.spline(a.vals, apply(muhat.mat,2,mean,na.rm=T)), x=a)$y
  mhat.mat <- matrix( rep(apply(muhat.mat,2,mean,na.rm=T),n), byrow=T, nrow=n)
  
  # form adjusted/pseudo outcome xi
  pseudo.out <- (y-muhat)/(pihat/varpihat) + mhat
  
  w.fn <- function(bw){ w.avals <- NULL; for (a.val in a.vals){
    a.std <- (a-a.val)/bw; kern.std <- kern(a.std)/bw
    w.avals <- c(w.avals, mean(a.std^2*kern.std)*(kern(0)/bw) /
                   (mean(kern.std)*mean(a.std^2*kern.std)-mean(a.std*kern.std)^2))
  }; return(w.avals/n) }
  hatvals <- function(bw){ approx(a.vals,w.fn(bw),xout=a,rule=2)$y }
  cts.eff.fn <- function(out,bw){
    approx(locpoly(a,out,bandwidth=bw,gridsize=1000),xout=a,rule=2)$y }
  # note: choice of bandwidth range depends on specific problem,
  # make sure to inspect plot of risk as function of bandwidth
  risk.fn <- function(h){ hats <- hatvals(h); mean( ((pseudo.out - cts.eff.fn(pseudo.out,bw=h))/(1-hats))^2) }
  risk.est <- sapply(bw.seq,risk.fn); 
  h.opt <- bw.seq[which.min(risk.est)]
  bw.risk <- data.frame(bw=bw.seq, risk=risk.est)
  # alternative approach:
  #h.opt <- optimize(function(h){ hats <- hatvals(h); mean( ((pseudo.out-cts.eff.fn(pseudo.out,bw=h))/(1-hats))^2) } ,
  #  bw.seq, tol=0.01)$minimum
  
  # estimate effect curve with optimal bandwidth
  est <- approx(locpoly(a,pseudo.out,bandwidth=h.opt),xout=a.vals)$y
  
  return(est)
}

ctseff_trim <- function(y,
                   a,
                   x, 
                   bw.seq, 
                   sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                   a.vals){
  
  kern <- function(t){ dnorm(t) }
  n <- dim(x)[1]
  
  # set up evaluation points & matrices for predictions
  a.min <- min(a.vals); a.max <- max(a.vals)
  xa.new <- rbind(cbind(x,a), cbind( x[rep(1:n,length(a.vals)),],a=rep(a.vals,rep(n,length(a.vals))) ))
  x.new <- xa.new[,-dim(xa.new)[2]]
  x <- data.frame(x); x.new <- data.frame(x.new); colnames(x) <- colnames(x.new)
  xa.new <- data.frame(xa.new)
  
  # estimate nuisance functions via super learner
  # note: other methods could be used here instead
  pimod <- SuperLearner(Y=a, X=data.frame(x), SL.library=sl.lib, newX=x.new)
  pimod.vals <- pimod$SL.predict
  pi2mod <- SuperLearner(Y=abs(a-pimod.vals[1:n]),X=x, SL.library=sl.lib, newX=x.new)
  pi2mod.vals <- pi2mod$SL.predict
  mumod <- SuperLearner(Y=y, X=cbind(x,a), SL.library=sl.lib,newX=xa.new)
  muhat.vals <- mumod$SL.predict
  
  # construct estimated pi/varpi and mu/m values
  a.std <- (xa.new$a-pimod.vals)/pi2mod.vals
  pihat.vals <- approx(density(a.std[1:n],na.rm = T)$x,density(a.std[1:n],na.rm = T)$y,xout=a.std,rule=2)$y
  pihat <- pihat.vals[1:n]; pihat.mat <- matrix(pihat.vals[-(1:n)], nrow=n,ncol=length(a.vals))
  varpihat <- predict(smooth.spline(a.vals, apply(pihat.mat,2,mean,na.rm=T)), x=a)$y
  varpihat.mat <- matrix( rep(apply(pihat.mat,2,mean,na.rm=T),n), byrow=T, nrow=n)
  muhat <- muhat.vals[1:n]; muhat.mat <- matrix(muhat.vals[-(1:n)], nrow=n,ncol=length(a.vals))
  mhat <- predict(smooth.spline(a.vals, apply(muhat.mat,2,mean,na.rm=T)), x=a)$y
  mhat.mat <- matrix( rep(apply(muhat.mat,2,mean,na.rm=T),n), byrow=T, nrow=n)
  
  # form adjusted/pseudo outcome xi
  pihat.stw <- (pihat/varpihat)
  pihat.stw[pihat.stw<0.1] <- 0.1
  pseudo.out <- (y-muhat)/pihat.stw + mhat
  
  w.fn <- function(bw){ w.avals <- NULL; for (a.val in a.vals){
    a.std <- (a-a.val)/bw; kern.std <- kern(a.std)/bw
    w.avals <- c(w.avals, mean(a.std^2*kern.std)*(kern(0)/bw) /
                   (mean(kern.std)*mean(a.std^2*kern.std)-mean(a.std*kern.std)^2))
  }; return(w.avals/n) }
  hatvals <- function(bw){ approx(a.vals,w.fn(bw),xout=a,rule=2)$y }
  cts.eff.fn <- function(out,bw){
    approx(locpoly(a,out,bandwidth=bw,gridsize=1000),xout=a,rule=2)$y }
  # note: choice of bandwidth range depends on specific problem,
  # make sure to inspect plot of risk as function of bandwidth
  risk.fn <- function(h){ hats <- hatvals(h); mean( ((pseudo.out - cts.eff.fn(pseudo.out,bw=h))/(1-hats))^2) }
  risk.est <- sapply(bw.seq,risk.fn); 
  h.opt <- bw.seq[which.min(risk.est)]
  bw.risk <- data.frame(bw=bw.seq, risk=risk.est)
  # alternative approach:
  #h.opt <- optimize(function(h){ hats <- hatvals(h); mean( ((pseudo.out-cts.eff.fn(pseudo.out,bw=h))/(1-hats))^2) } ,
  #  bw.seq, tol=0.01)$minimum
  
  # estimate effect curve with optimal bandwidth
  est <- approx(locpoly(a,pseudo.out,bandwidth=h.opt),xout=a.vals)$y
  
  return(est)
}

############### CBPS approach
CBPS.fun.dose<-function(a,
                        model = CBPS_model) {
  
  data.a <- data.frame(cbind(treat = a))
  E.a <- predict(model,data.a)[[1]]
  
  return(c(mean(E.a)))
}

run_sim_dose_CBPS<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals) {
  
  cbps_data <- cbind(treat, c)
  
  cbps.wt <- tryCatch(
    {CBPS(treat ~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6, data = cbps_data, method = "over")
    },
    error = function(cond) {CBPS(treat ~ cf1 + cf2 + cf3 + cf4 + cf5 + cf6, data = cbps_data, method = "exact")
    })
  
  CBPS_model <- SuperLearner(Y = Y, X = data.frame(treat), obsWeights = cbps.wt$weights, SL.library = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  
  CBPS_data <- sapply(a.vals,
                      CBPS.fun.dose,
                      model = CBPS_model)
  
  return(CBPS_data)
}


true_model<-function(i,data.generation=data.generate,outcome_sd=50,sample_size=2000,a.vals,gps_spec){
  simulated.data<-data.generation(sample_size=sample_size,seed=i,outcome_sd=outcome_sd,gps_spec=gps_spec)
  simulated.data$treat<-simulated.data$treat-20
  true_model<-lm(Y~treat+I(treat^3)+(cf1 +cf4 + cf5 + I(cf3^2))*treat+cf1+cf2+cf3+cf4+cf5+cf6,simulated.data)
  coefficients<-true_model$coefficients
  true.noerror<-matrix(NA,nrow=length(a.vals),ncol=1)
  for (a_i in a.vals){
    true.noerror[(a_i-min(a.vals))+1]<-coefficients[1]+coefficients[7]+(coefficients[2]+coefficients[14])*(a_i-20)+coefficients[3]*(a_i-20)^3
  }
  return(true.noerror)
}
