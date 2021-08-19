#' @title
#' Create pseudo population using matching casual inference approach
#'
#' @description
#' Generates pseudo population based on matching casual inference method.
#'
#' @param dataset A list with 6 elements. Including An original dataset as well
#'  as helper vectors from estimating GPS. See [compile_pseudo_pop()] for more
#'  details.
#' @param bin_seq Sequence of w (treatment) to generate pseudo population. If
#' NULL is passed the default value will be used, which is
#' `seq(min(w)+delta_n/2,max(w), by=delta_n)`.
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric (default) and non-parametric.
#' @param nthread Number of available cores.
#' @param optimized_compile Option to activate optimized compilation.
#' @param ...  Additional arguments passed to the function.
#'
#' @return
#' Returns data.table of matched set.
#'
#' @keywords internal
#'
create_matching <- function(dataset, bin_seq = NULL, gps_model = "parametric",
                            nthread = 1, optimized_compile, ...){

  # dataset content: dataset, e_gps_pred, e_gps_std_pred, w_resid, gps_mx, w_mx

  # Passing packaging check() ----------------------------
  delta_n <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  matching_fun <- get(matching_fun)

  gps_mx <- dataset[[5]]
  w_mx <- dataset[[6]]

  if (is.null(bin_seq)){

    bin_num<-seq(w_mx[1]+delta_n/2, w_mx[2], by = delta_n)
    used_bin <- "Default"

  } else {

    bin_num <- bin_seq
    used_bin <- "User defined"

  }

    logger::log_debug(used_bin, "bin seq is used. Min: {min(bin_num)} ",
                        "Max: {max(bin_num)}, count: {length(bin_num)}.")

    logger::log_debug("Started generating matched set ...")
    st_t_m <- proc.time()

    lfp <- get_options("logger_file_path")

    p_c_t_s <- proc.time()
    cl <- parallel::makeCluster(nthread, type="PSOCK",
                                outfile= lfp)


    # seed_val <- .Random.seed
    # parallel::clusterSetRNGStream(cl = cl, iseed = seed_val)

    parallel::clusterEvalQ(cl, {library("CausalGPS")})
    p_c_t_e <- proc.time()

    logger::log_debug("Wall clock time to create cluster with ",
                      "{nthread} core(s): {p_c_t_e[[3]] - p_c_t_s[[3]]} s.")
    parallel::clusterExport(cl=cl,
                          varlist = c("bin_num", "matching_fun", "dataset",
                                      "gps_mx", "w_mx", "delta_n", "scale",
                                      "nthread", "compute_closest_wgps",
                                      "compute_resid", "compute_density"),
                          envir=environment())

    matched_set <-  parallel::parLapply(cl,
                                        bin_num,
                                        matching_fun,
                                        dataset=dataset[[1]],
                                        e_gps_pred = dataset[[2]],
                                        e_gps_std_pred = dataset[[3]],
                                        w_resid=dataset[[4]],
                                        gps_mx = gps_mx,
                                        w_mx = w_mx,
                                        gps_model = gps_model,
                                        delta_n = delta_n,
                                        scale = scale,
                                        nthread = nthread,
                                        optimized_compile = optimized_compile)
    parallel::stopCluster(cl)

  e_t_m <- proc.time()
  logger::log_debug("Finished generating matched set (Wall clock time:  ",
                    " {(e_t_m - st_t_m)[[3]]} seconds).")

  if (!optimized_compile){
    return(data.table(do.call(rbind,matched_set)))
  } else {

    cp_original_data <- dataset[[1]]
    bind_matched_set = do.call(rbind,matched_set)
    freq_table = as.data.frame(table(bind_matched_set))

    index_of_data <- as.numeric(as.character(freq_table[1][,1]))
    added_count <- as.numeric(as.character(freq_table[2][,1]))

    for (i in seq(1,nrow(freq_table))){
      (cp_original_data[index_of_data[i],"counter"] <-
          cp_original_data[index_of_data[i],"counter"] + added_count[i])
    }

    return(data.table(cp_original_data))
  }
}
