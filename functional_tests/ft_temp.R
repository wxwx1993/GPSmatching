m_d <- generate_syn_data(sample_size = 500)
pseuoo_pop <- generate_pseudo_pop(m_d$Y,
                                  m_d$treat,
                                  m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                  ci_appr = "matching",
                                  pred_model = "sl",
                                  gps_model = "parametric",
                                  bin_seq = NULL,
                                  trim_quantiles = c(0.01,0.99),
                                  optimized_compile = TRUE,
                                  use_cov_transform = FALSE,
                                  transformers = list(),
                                  sl_lib = c("m_xgboost"),
                                  params = list(xgb_nrounds=c(10,20,30),
                                                xgb_eta=c(0.1,0.2,0.3)),
                                  nthread = 1,
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type= "mean",
                                  max_attempt = 1,
                                  matching_fun = "matching_l1",
                                  delta_n = 1,
                                  scale = 0.5)


# ------------------------------------------------------------------------------

# Example code for implementation
set.seed(431390)
n_subset <- 3
n <- 5
a <- sample(1:n,n_subset)
e <- 1:n_subset
cd <- rep(0,n_subset)
b <- sample(1:n)
f <- 1:n
sc <- 0.5
thresh <- 0.5
true_out <- c(2,1,2,3,3)
nthread <- 1
estimate_out <- compute_closest_2SM_helper(a,b,cd,e,f,sc,nthread,thresh)
setequal(true_out,estimate_out) # should be TRUE

set.seed(187)
a <- 1:5
b <- 1:10
c <- (1:5)*0.1
e <- runif(5)
f <- runif(10)
d <- 4
sc <- 0.5
nthread <- 1

compute_closest_wgps_cluster(a,b,c,d,e,f,thresh_cluster = 0.1,
                             thresh_min = 0.5,
                             sc = sc,
                             nthread = 1
)

# ------------------------------------------------------------------------------


compute_closest_2SM_helper <- function(a,b,cd,e,f,sc,nthread,thresh=0.1){

  dat_ori <- data.frame("b"=b,"f"=f)
  dat_subset <- data.frame("a"=a,"e"=e,"cd"=cd)


  size_a <- nrow(dat_subset)
  size_b <- nrow(dat_ori)

  out <- rep(NA,size_b)

  dat_subset$ID <- 1:size_a

  for(i in 1:size_b){ # for each units in original data b


    tmp_val = 0;
    min_index = 0;
    min_val = thresh + 1;
    subtract_val = 0;

    dat_subset$distance <- abs(dat_ori$f[i] - dat_subset$e)
    dat_subset$cluster_rank <- rank(dat_subset$distance,ties.method="min")

    for(target_idx in sort(unique(dat_subset$cluster_rank))){

      if(min_val > thresh){ # check whether to move onto next cluster or not
        dat_subset_subset <- dat_subset[dat_subset$cluster_rank == target_idx, ]
        size_aa <- nrow(dat_subset_subset)
        for(j in 1:size_aa) { # for each units in subset data a

          subtract_val = (dat_ori$b[i]-dat_subset_subset$a[j])*sc; # comparison of PS * lambda
          if (subtract_val < 0) subtract_val <- abs(subtract_val);
          tmp_val =  subtract_val + dat_subset$cd[j]; # add distance between exposure level w

          if (j==1){
            min_val = tmp_val;
            min_index = dat_subset_subset$ID[j];
            # continue;
          }

          if (tmp_val < min_val){
            min_val = tmp_val;
            min_index = dat_subset_subset$ID[j];
          }
        } # end of loop j: obtained min_val and min_index
      } else{ break;} # end of if loop
    } # end of target_idx loop (matching)
    out[i] = min_index
  }
  return(out)
}
