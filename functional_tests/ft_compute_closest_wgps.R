# Compare with Scale 1 and 0.9999


set.seed(1387)
a <- runif(20)
b <- runif(30)
c <- (1:20)*0.1
d <- 0.1
sc <- 1
nthread <- 1

wm_1 <- compute_closest_wgps(a,b,c,d,sc,nthread)
wm_1


set.seed(1387)
a <- runif(20)
b <- runif(30)
c <- (1:20)*0.0
d <- 0.0
sc <- 0.9999
nthread <- 1

wm_2 <- compute_closest_wgps(a,b,c,d,sc,nthread)
wm_2



