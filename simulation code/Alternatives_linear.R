library(nnet)
library("MASS")
library("SuperLearner")
library("xgboost")
library("earth")
library("ranger")
library("gam")
require(KernSmooth)
#####including GPS as covariates
GPScova.fun.dose<-function(data_set,
                           c,
                           a,
                           GPS_mod,
                           model=flexible_model){
  #p.a<-dnorm(a,mean = predict(GPS_mod,simulated.data),sd=summary(GPS_mod)$sigma)
  data_set <- cbind(data_set,c)
  p.a <- dnorm(a,mean = predict(GPS_mod, data_set),sd=summary(GPS_mod)[["sigma"]])
  data.a<-data.frame(cbind(treat=a,GPS=p.a))
  E.a<-predict(model,data.a)[[1]]
  
  return(c(mean(E.a)))
}

run_sim_dose_cova<-function(Y,
                            treat,
                            c,
                            sl.lib=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  GPS_mod<-lm(treat ~ cf1+cf2+cf3+cf4+cf5+cf6 , cbind(treat,c))
  predict<-predict(GPS_mod,cbind(treat,c))
  GPS<-dnorm(treat,mean = predict,sd=summary(GPS_mod)[["sigma"]])
   
  data_set<-data.frame(cbind(treat=treat,GPS=GPS))
  
  flexible_model<-SuperLearner(Y=Y, X=data_set, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  
  GPScova_data <- sapply(a.vals,
                         GPScova.fun.dose,
                         data_set=data_set,
                         c=c,
                         GPS_mod=GPS_mod,
                         model=flexible_model)
  return(GPScova_data)
}


##############IPTW, or call MSMs
IPW.fun.dose<-function(a,
                       model=IPW_model){
  #p.a<-dnorm(a,mean = predict(GPS_mod,simulated.data),sd=summary(GPS_mod)$sigma)
  data.a<-data.frame(cbind(treat=a))
  E.a<-predict(model,data.a)[[1]]
  #E.a<-predict(model,data.a)
  
  #E.a <- predict(model,data.frame(treat=a),weights =IPW)
  return(c(mean(E.a)))
}

run_sim_dose_IPTW<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  GPS_mod<-lm(treat ~ cf1+cf2+cf3+cf4+cf5+cf6 , cbind(treat,c))
  predict<-predict(GPS_mod,cbind(treat,c))
  GPS<-dnorm(treat,mean = predict,sd=summary(GPS_mod)[["sigma"]])
  
  Nm<-dnorm(treat,mean=mean(treat,na.rm=T),sd=sd(treat,na.rm=T))
  IPW<-Nm/(GPS)
  #if (sum(simulated.data$IPW>10)>0){simulated.data[which(simulated.data$IPW>10),]$IPW<-10}
  
  IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), obsWeights = IPW, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  #IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), SL.library=sl.lib)
  #IPTW_model<-lm(Y~treat+I(treat^2)+I(treat^3),weights=IPW)
  
  IPTW_data<- sapply(a.vals,
                     IPW.fun.dose,
                     model=IPTW_model)
  
  return(IPTW_data)
}

run_sim_dose_IPTW_trim<-function(Y,
                            treat,
                            c,
                            sl.lib = c("SL.xgboost","SL.earth","SL.gam","SL.ranger"),
                            a.vals){
  GPS_mod<-lm(treat ~ cf1+cf2+cf3+cf4+cf5+cf6 , cbind(treat,c))
  predict<-predict(GPS_mod,cbind(treat,c))
  GPS<-dnorm(treat,mean = predict,sd=summary(GPS_mod)[["sigma"]])
  
  Nm<-dnorm(treat,mean=mean(treat,na.rm=T),sd=sd(treat,na.rm=T))
  IPW<-Nm/(GPS)
  IPW[IPW >10] <- 10
  #if (sum(simulated.data$IPW>10)>0){simulated.data[which(simulated.data$IPW>10),]$IPW<-10}
  
  IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), obsWeights = IPW, SL.library=c("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
  #IPTW_model<-SuperLearner(Y=Y, X= data.frame(treat), SL.library=sl.lib)
  #IPTW_model<-lm(Y~treat+I(treat^2)+I(treat^3),weights=IPW)
  
  IPTW_data<- sapply(a.vals,
                     IPW.fun.dose,
                     model=IPTW_model)
  
  return(IPTW_data)
}


##############Kennedy
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
  #pimod <- SuperLearner(Y=a, X=data.frame(x), SL.library=sl.lib, newX=x.new)
  #pimod.vals <- pimod$SL.predict
  #pi2mod <- SuperLearner(Y=abs(a-pimod.vals[1:n]),X=x, SL.library=sl.lib, newX=x.new)
  #pi2mod.vals <- pi2mod$SL.predict
  GPS_mod<-lm(a ~ cf1+cf2+cf3+cf4+cf5+cf6 , cbind(a,x))
  predict<-predict(GPS_mod,cbind(a,x))
  pihat.vals<-dnorm(xa.new$a,mean = predict,sd=summary(GPS_mod)[["sigma"]])
  
  mumod <- SuperLearner(Y=y, X=cbind(x,a), SL.library=sl.lib,newX=xa.new)
  muhat.vals <- mumod$SL.predict
  
  # construct estimated pi/varpi and mu/m values
  #a.std <- (xa.new$a-pimod.vals)/pi2mod.vals
  #pihat.vals <- approx(density(a.std[1:n],na.rm = T)$x,density(a.std[1:n],na.rm = T)$y,xout=a.std,rule=2)$y
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
  GPS_mod<-lm(treat ~ cf1+cf2+cf3+cf4+cf5+cf6 , cbind(a,x))
  predict<-predict(GPS_mod, cbind(a,x))
  pihat.vals<-dnorm(xa.new$a,mean = predict,sd=summary(GPS_mod)[["sigma"]])
  
  mumod <- SuperLearner(Y=y, X=cbind(x,a), SL.library=sl.lib,newX=xa.new)
  muhat.vals <- mumod$SL.predict
  
  # construct estimated pi/varpi and mu/m values
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


MSE_AB<-function(data.list, true.response){
  
  sim.diff <- lapply(1:length(data.list), function(j){
      pred.response <- (data.list[[j]])
      return(pred.response - true.response)
    })
 
  DR.AB <- abs(rowMeans(apply(simplify2array(sim.diff), 1:2, mean)))
  DR.MSE <- sqrt(rowMeans(apply(simplify2array(sim.diff)^2, 1:2, mean)))
  
  return(list(rbind(DR.AB,DR.MSE),c(mean(DR.AB),mean(DR.MSE))))
}

