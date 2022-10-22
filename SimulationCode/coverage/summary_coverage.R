# generate Table S.2 and S.3

## Table S.2: Coverage rate. We estimate the GPS using parametric linear regression models. 
## All results are based on S = 500 replicates
coverage200<-NULL
coverage1000<-NULL
coverage5000<-NULL

k = 1
for (i in c(1:7)) {
f <- list.files("~/coverage/200",
                pattern = paste0("\\coverage200gps_spec", i),
                full.names = TRUE)

coverage <- do.call("rbind",lapply(f, function(i) {
  readRDS(i)[3,]}))
coverage200[k] <- mean(rowMeans(coverage, na.rm = TRUE))
#median(rowMeans(coverage, na.rm = TRUE))

# sim 1
f <- list.files("~/coverage/1000",
                pattern = paste0("\\coverage1000gps_spec", i),
                full.names = TRUE)

coverage <- do.call("rbind",lapply(f, function(i) {
  readRDS(i)[3,]}))
coverage1000[k] <- mean(rowMeans(coverage, na.rm = TRUE))
#median(rowMeans(coverage, na.rm = TRUE))

f <- list.files("~/coverage/5000",
                pattern = paste0("\\coverage5000gps_spec", i),
                full.names = TRUE)

coverage <- do.call("rbind",lapply(f, function(i) {
  readRDS(i)[3,]}))
mean(rowMeans(coverage, na.rm = TRUE))
coverage5000[k] <- mean(rowMeans(coverage, na.rm = TRUE))
k = k+1
#median(rowMeans(coverage, na.rm = TRUE))
}

mean(coverage200gps_spec1cova_spec1process0[3,], na.rm = T)

plot(coverage200gps_spec1cova_spec1process0[2,], type = "l")
lines(coverage200gps_spec1cova_spec1process0[1,])

coverage200<-NULL
coverage1000<-NULL
coverage5000<-NULL

k = 1
for (i in c(1:7)) {
  f <- list.files("~/coverage/200_linear_m2",
                  pattern = paste0("\\coverage200gps_spec", i),
                  full.names = TRUE)
  
  coverage <- do.call("rbind",lapply(f, function(i) {
    readRDS(i)[3,]}))
  coverage200[k] <- mean(rowMeans(coverage, na.rm = TRUE))
  #median(rowMeans(coverage, na.rm = TRUE))
  
  # sim 1
  f <- list.files("~/coverage/1000_linear_m2",
                  pattern = paste0("\\coverage1000gps_spec", i),
                  full.names = TRUE)
  
  coverage <- do.call("rbind",lapply(f, function(i) {
    readRDS(i)[3,]}))
  coverage1000[k] <- mean(rowMeans(coverage, na.rm = TRUE))
  #median(rowMeans(coverage, na.rm = TRUE))
  
  f <- list.files("~/coverage/5000_linear_m2",
                  pattern = paste0("\\coverage5000gps_spec", i),
                  full.names = TRUE)
  
  coverage <- do.call("rbind",lapply(f, function(i) {
    readRDS(i)[3,]}))
  mean(rowMeans(coverage, na.rm = TRUE))
  coverage5000[k] <- mean(rowMeans(coverage, na.rm = TRUE))
  k = k+1
  #median(rowMeans(coverage, na.rm = TRUE))
}


## Table S.3: Coverage rate. We vary the size m with sample size N = 5000. All results are based on S = 500 replicates
coverage_m<-NULL

k = 1
for (i in 1:8) {
  f <- list.files("~/coverage/5000_linear_m",
                  pattern = paste0("\\coverage5000gps_spec1cova_spec1m", i),
                  full.names = TRUE)
  
  coverage <- do.call("rbind",lapply(f, function(i) {
    readRDS(i)[3,]}))
  coverage_m[k] <- mean(rowMeans(coverage, na.rm = TRUE))
  k = k+1
  #median(rowMeans(coverage, na.rm = TRUE))
}


