#' @title
#' Create pseudo population using matching casual inference approach
#'
#' @description
#' Generates pseudo population based on matching casual inference method.
#'
#' @param dataset A list with 6 elements. Including An original dataset as well
#'  as helper vectors from estimating GPS. See [compile_pseudo_pop()] for more
#'  details.
#' @param nthread Number of available cores.
#' @param ...  Additional arguments passed to the function.
#'
#' @return
#' Returns data.table of matched set.
#' @export
#'
create_matching <- function(dataset, nthread = 1, ...){

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

  bin_num<-seq(w_mx[1]+delta_n/2, w_mx[2], by = delta_n)

  logger::log_debug("Started generating matched set (num bins: {length(bin_num)}) ...")
  st_t_m <- proc.time()

  cl <- parallel::makeCluster(nthread)
  matched_set <-  parallel::parLapply(cl,
                                      bin_num,
                                      matching_fun,
                                      dataset=dataset[[1]],
                                      e_gps_pred = dataset[[2]],
                                      e_gps_std_pred = dataset[[3]],
                                      w_resid=dataset[[4]],
                                      gps_mx = gps_mx,
                                      w_mx = w_mx,
                                      delta_n = delta_n,
                                      scale = scale,
                                      nthread = nthread)
  parallel::stopCluster(cl)

  logger::log_debug("Started generating matched set ...")

  e_t_m <- proc.time()
  logger::log_debug("Finished generating matched set (Wall clock time:  ",
                    " {(e_t_m - st_t_m)[[3]]} seconds).")

  return(data.table(do.call(rbind,matched_set)))
}
