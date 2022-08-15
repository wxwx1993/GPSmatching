# Question: How to run generate_pseudo_pop in lapply and parLapply?

# Answer: See the following code. However, parLapply is not recommended. Because
# causal GPS internally can use all available cores internally so you might not
# gain any performance by adding another parallel layer.

# You can either Use a wrapper function to run generate_syn_data in
# parallel or use lapply directly and provide the list variable at first,
# then the function and then other variables.


delta_n <- c(1:10)*0.1
m_d <- generate_syn_data(sample_size = 200)

wrapper_func <- function(delta_n){
  pseudo_pop <- generate_pseudo_pop(m_d$Y,
                                    m_d$treat,
                                    m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=c(10,20,30),
                                                  xgb_eta=c(0.1,0.2,0.3)),
                                    nthread = 1,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = delta_n,
                                    scale = 0.5)

  return(pseudo_pop)
}

# Function to collect the results
print_mean_covariate_balance <- function(delta_n, object_list){
  m_cov <- unlist(lapply(object_list,
                         function(x){x$adjusted_corr_results$mean_absolute_corr}))
  results <- data.frame(delta_n, m_cov)
  print(results)
}


# with lapply ------------------------------------------------------------------
pseudo_pop_list_1  <- lapply(delta_n, wrapper_func)

# mean covariate balance
print_mean_covariate_balance(delta_n = delta_n, object_list = pseudo_pop_list_1)


# with parLapply ---------------------------------------------------------------
cl <- parallel::makeCluster(10, type="PSOCK")
parallel::clusterExport(cl=cl,
                        varlist = c("generate_pseudo_pop",
                                    "wrapper_func",
                                    "m_d"
                        ),
                        envir=environment())

pseudo_pop_list_2  <- parallel::parLapply(cl,delta_n, wrapper_func)
parallel::stopCluster(cl)

# mean covariate balance
print_mean_covariate_balance(delta_n = delta_n, object_list = pseudo_pop_list_2)


