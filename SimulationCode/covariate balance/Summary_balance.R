# Figure 1: Absolute Correlations
library(xtable)
library("data.table")

########5000
spec = 1

data.name <- list.files("~/balance",
                        pattern = paste0("MAC_match5000gps_spec",spec,"cova_spec1"), full.names = FALSE)

MAC5000<-list()
for (i in 1:length(data.name)){
  load(paste0("~/balance/",data.name[i]))
  MAC5000[[i]]<-MAC_matched.df
  assign( data.name[i],MAC_matched.df)
}

MAC5000_df <- rbindlist(MAC5000)

which.min(rowMeans(MAC5000_df[,1:6]))
rowMeans(MAC5000_df[,1:6])[which.min(rowMeans(MAC5000_df[,1:6]))]
rowMeans(MAC5000_df[,7:12])[which.min(rowMeans(MAC5000_df[,7:12]))]
rowMeans(MAC5000_df[,13:18])[which.min(rowMeans(MAC5000_df[,13:18]))]


plot(as.numeric(MAC5000_df[1,7:12]),type="l",ylim=c(0,0.5))
lines(as.numeric(MAC5000_df[which.min(rowMeans(MAC5000_df[,1:6])),1:6]),col="red")
lines(as.numeric(MAC5000_df[1,13:18]),col="blue")

pdf(paste0("balance_matched_all_5000_3.pdf"),width=14,height=15)
par(mfrow=c(2,3))
par(mar=c(8.1,4.1,4.1,4.1))
par(mgp=c(5,1,0))
num = 0
for (i in c(1:6)){
spec = i
num = num + 1
data.name <- list.files("~/balance",
                        pattern = paste0("MAC_match5000gps_spec",spec,"cova_spec1"), full.names = FALSE)
data.name2 <- list.files("~/balance",
                        pattern = paste0("MAC_cbps5000gps_spec",spec,"cova_spec1"), full.names = FALSE)
MAC5000<-list()
MAC_cbps5000<-list()
for (i in 1:length(data.name)){
  load(paste0("~/balance/",data.name[i]))
  MAC5000[[i]]<-MAC_matched.df
  assign( data.name[i],MAC_matched.df)
  
}

load(paste0("~/balance/",data.name2))

MAC5000_df <- rbindlist(MAC5000)
min.balance1<-which.min(rowMeans(MAC5000_df[,1:6]))
cor_orgin1<-MAC5000_df[min.balance1,7:12]
cor_matched1<-MAC5000_df[min.balance1,1:6]
cor_weight1<-MAC5000_df[min.balance1,13:18]
cor_cbps<-MAC_cbps.df[7:12]

order.1<-sort(as.numeric(MAC5000_df[min.balance1,7:12]), index.return =T)$ix
histcolors = c(rgb(0,0,1,.8), rgb(0,1,0,.8),rgb(1,0,0,.8),rgb(0.8,0.2,0,.8))
plot((as.numeric(cor_orgin1))[order.1],c(1,2,3,4,5,6),yaxt="n",xlab="Absolute Correlation", main = paste0("Scenario ",num),
     ylab="",type = "b",lty=2,pch=2,xlim=c(0,0.5), col = histcolors[4],lwd=3, cex.lab = 3,cex.axis=2,cex.main=3)
lines((as.numeric(cor_matched1))[order.1],c(1,2,3,4,5,6), col = histcolors[1],lwd=3,type = "b",pch=1)
lines((as.numeric(cor_cbps))[order.1],c(1,2,3,4,5,6), col = histcolors[2],lwd=3,type = "b",pch=1)
#lines(((mean_cor_weighted))[order.1],c(1,2,3,4,5,6), col = histcolors[4],lwd=3)
#lines(((mean_cor_weighted_unstable))[order.1],c(1,2,3,4,5,6), col = histcolors[3],lwd=3)
abline(v=0.1,lty=3,lwd=3)
axis(2, at=c(1,2,3,4,5,6), labels=c("cf1","cf2","cf3","cf4","cf5","cf6")[order.1],cex.axis=2,las=1)
legend("bottomright", c("Matched","CBPS", "Unadjusted"), 
       xpd = TRUE, horiz = F, inset = c(0, 0.0), bty = "n", col = histcolors[c(1,2,4)], 
       lwd = c(10, 10, 10), lty = c(1, 1, 1), cex = 2.5)

}
dev.off()


