set.seed(2219)
a <- sample(1:1000000000000000, 100000000, replace = FALSE)

set.seed(22109)
b <- sample(1:1000000000000000, 10000, replace = FALSE)


nthread <- 12

# Not that the (a) input for the following function should be sorted.
a <- sort(a)
st_time_ord <- proc.time()
val2 <- compute_closest_wgps_helper_no_sc(a,b,nthread)
#  print(val2)
et_time_ord <- proc.time()

print(paste0("Wall clock time to ordinary approach:",
             (et_time_ord -   st_time_ord)[[3]]," seconds."))


st_time_binary <- proc.time()
val3 <- compute_closest_wgps_no_sc_binary_search(a, b, nthread)
#print(val3)
et_time_binary <- proc.time()

print(paste0("Wall clock time to binary approach:",
             (et_time_binary -   st_time_binary)[[3]]," seconds."))

print(identical(val2, val3))
plot(val2, val3)
