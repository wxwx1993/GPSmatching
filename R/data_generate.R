library(MASS)

data_generate<-function(sample_size=1000,seed=300,outcome_sd = 10,gps_spec= 1, cova_spec=1){

  options(digits=4) # only print 4 sig digits
  set.seed(seed)
  size<-sample_size
  #pre-treatment variables (confounders)
  cf<-mvrnorm(n=size,mu=c(0,0,0,0),Sigma=matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),ncol=4))
  cf5<-sample(c((-2):2),size,replace = T)
  cf6<-runif(size,min=-3,max=3)

  if (gps_spec==1){
    treat<-(-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6)*9 + 17 + rnorm(size,sd=5)
 }else if (gps_spec==2){
   treat<-(-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6)*15 +22 + rt(size,2)
   treat[which(treat<(-5))]<-(-5)
   treat[which(treat>(25))]<-(25)
 }else if (gps_spec==3){
   treat<-(-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6)*9 + 1.5*cf[,3]^2  + rnorm(size,mean=0,5) +15
 }else if (gps_spec==4){
   treat<-49*exp((-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6))/(1+exp((-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6)))-6 + rnorm(size,sd=5)
 }else if (gps_spec==5){
   treat<-42/(1+exp((-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6))) -18 + rnorm(size,sd=5)
 }else if (gps_spec==6){
   treat<-log(abs(-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6))*7 + 13 + rnorm(size,sd=4)
 }else if (gps_spec==7){
   treat<- (-0.8+0.1*cf[,1]+0.1*cf[,2]- 0.1*cf[,3] + 0.2*cf[,4] + 0.1*cf5 + 0.1*cf6)*15 + 22 + rt(size,2) #+ rcauchy(size)
 }
  #produce outcome Y
  Y<-as.numeric()
  for (i in 1:size){
    #Y[i]<- 200 + 10*treat[i] + 13.7*(2*cf[i,1] + cf[i,2] + cf[i,3] + cf[i,4])  + rnorm(1,mean=0,sd=1)
    Y[i]<-(-(1 + (sum(c(0.2, 0.2, 0.3, -0.1)*cf[i,]))*10 - 2*cf5[i]-2*cf6[i]  + (treat[i]-20)*(0.1 +0.1*cf[i,4] +0.1*cf5[i] +0.1*cf[i,3]^2 - 0.13^2*(treat[i]-20)^2)))  + rnorm(1,mean=0,sd=outcome_sd)
  }
  if (cova_spec==1){
    cf = cf
  #Kang 2007
  }else if (cova_spec==2){
    cf[,1] <- exp(cf[,1]/2)
    cf[,2] <- (cf[,2]/(1+exp(cf[,1]))) + 10
    cf[,3] <- (cf[,1]*cf[,3]/25 + 0.6)^3
    cf[,4] <- (cf[,2] + cf[,4] + 20)^2
  }

  #treat<-treat+20
  simulated.data<-data.frame(cbind(Y,treat,cf, cf5, cf6))
  colnames(simulated.data)[3:8]<-c("cf1","cf2","cf3","cf4","cf5","cf6")
  return(simulated.data)
}
