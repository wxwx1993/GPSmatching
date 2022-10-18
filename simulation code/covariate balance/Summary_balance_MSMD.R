# Figure S.3: Block Absolute Standardized Bias (BASB) where we categorize the exposure range by quintile (K = 5)

library(xtable)
library("data.table")
########5000
spec = 1

data.name <- list.files("~/Dropbox/continuous GPS/JASA resubmission/Simulation/MSMD/",
                        pattern = paste0("MSMD5000gps_spec",spec,"cova_spec1"), full.names = FALSE)

MSMD5000<-list()
for (i in 1:length(data.name)){
  load(paste0("~/Dropbox/continuous GPS/JASA resubmission/Simulation/MSMD/",data.name[i]))
  MSMD5000 <- append(MSMD5000, MSMD_matched)
  #assign(data.name[i],MSMD_matched.df)
}

MSMD5000_df <- do.call("rbind", lapply(MSMD5000, function(list) {list[[1]]}))

which.min(rowMeans(MSMD5000_df[,1:6]))
rowMeans(MSMD5000_df[,1:6])[which.min(rowMeans(MSMD5000_df[,1:6]))]
rowMeans(MSMD5000_df[,7:12])[which.min(rowMeans(MSMD5000_df[,7:12]))]
#rowMeans(MSMD5000_df[,13:18])[which.min(rowMeans(MSMD5000_df[,13:18]))]


plot(as.numeric(MSMD5000_df[1,7:12]),type="l",ylim=c(0,0.5))
lines(as.numeric(MSMD5000_df[which.min(rowMeans(MSMD5000_df[,1:6])),1:6]),col="red")
#lines(as.numeric(MAC5000_df[1,13:18]),col="blue")

########5000

for (k in 1:5) {
pdf(paste0("balance_matched_all_5000_MSMD", k, ".pdf"),width=14,height=15)
par(mfrow=c(2,3))
par(mar=c(8.1,4.1,4.1,4.1))
par(mgp=c(5,1,0))
num = 0
for (i in c(1,7,3,4,5,6)){
  spec = i
  num = num + 1
  data.name <- list.files("~/Dropbox/continuous GPS/JASA resubmission/Simulation/MSMD/",
                          pattern = paste0("MSMD5000gps_spec",spec,"cova_spec1"), full.names = FALSE)
  MSMD5000<-list()
  for (j in 1:length(data.name)){
    load(paste0("~/Dropbox/continuous GPS/JASA resubmission/Simulation/MSMD/",data.name[j]))
    MSMD5000 <- append(MSMD5000, MSMD_matched)
    #assign(data.name[i],MSMD_matched.df)
  }
  
  MSMD5000_df <- do.call("rbind", lapply(MSMD5000, function(list) {list[[1]]}))
  
  min.balance1<-which.min(rowMeans(MSMD5000_df[,1:6]))
  #cor_orgin1<-MSMD5000_df[min.balance1,7:12]
  #cor_matched1<-MSMD5000_df[min.balance1,1:6]
  
  cor_matched1 <- MSMD5000[[min.balance1]][[2]][k,]
  cor_orgin1 <- MSMD5000[[min.balance1]][[3]][k,]

  order.1<-sort(as.numeric(MSMD5000_df[min.balance1,7:12]), index.return =T)$ix
  histcolors = c(rgb(0,0,1,.8), rgb(0,1,0,.8),rgb(1,0,0,.8),rgb(0.8,0.2,0,.8))
  if (i != 7) {
  plot((as.numeric(cor_orgin1))[order.1],c(1,2,3,4,5,6),yaxt="n",xlab="BASB", main = paste0("Scenario ",num),
       ylab="",type = "b",lty=2,pch=2,xlim=c(0,0.6), col = histcolors[4],lwd=3, cex.lab = 3,cex.axis=2,cex.main=3)
  lines((as.numeric(cor_matched1))[order.1],c(1,2,3,4,5,6), col = histcolors[1],lwd=3,type = "b",pch=1)
  #lines((as.numeric(cor_cbps))[order.1],c(1,2,3,4,5,6), col = histcolors[2],lwd=3,type = "b",pch=1)
  #lines(((mean_cor_weighted))[order.1],c(1,2,3,4,5,6), col = histcolors[4],lwd=3)
  #lines(((mean_cor_weighted_unstable))[order.1],c(1,2,3,4,5,6), col = histcolors[3],lwd=3)
  abline(v=0.2, lty=3, lwd=3)
  axis(2, at=c(1,2,3,4,5,6), labels=c("cf1","cf2","cf3","cf4","cf5","cf6")[order.1],cex.axis=2,las=1)
  legend("bottomright", c("Matched", "Unadjusted"), 
         xpd = TRUE, horiz = F, inset = c(0, 0.0), bty = "n", col = histcolors[c(1,4)], 
         lwd = c(10, 10), lty = c(1, 1), cex = 2.5)
  } else if (i == 7) {
    plot((as.numeric(cor_orgin1))[order.1],c(1,2,3,4,5,6),yaxt="n",xlab="BASB", main = paste0("Scenario ",num),
         ylab="",type = "b",lty=2,pch=2,xlim=c(0,1), col = histcolors[4],lwd=3, cex.lab = 3,cex.axis=2,cex.main=3)
    lines((as.numeric(cor_matched1))[order.1],c(1,2,3,4,5,6), col = histcolors[1],lwd=3,type = "b",pch=1)
    #lines((as.numeric(cor_cbps))[order.1],c(1,2,3,4,5,6), col = histcolors[2],lwd=3,type = "b",pch=1)
    #lines(((mean_cor_weighted))[order.1],c(1,2,3,4,5,6), col = histcolors[4],lwd=3)
    #lines(((mean_cor_weighted_unstable))[order.1],c(1,2,3,4,5,6), col = histcolors[3],lwd=3)
    abline(v=0.2, lty=3, lwd=3)
    axis(2, at=c(1,2,3,4,5,6), labels=c("cf1","cf2","cf3","cf4","cf5","cf6")[order.1],cex.axis=2,las=1)
    legend("bottomright", c("Matched", "Unadjusted"), 
           xpd = TRUE, horiz = F, inset = c(0, 0.0), bty = "n", col = histcolors[c(1,4)], 
           lwd = c(10, 10), lty = c(1, 1), cex = 2.5)
  }
}
dev.off()
}



