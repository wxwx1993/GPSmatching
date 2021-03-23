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


  # TODO: Using more than available threads sometimes cause forking issue.
  # Multiplication of threads in the following mclapply and matching_fun should
  # be less than provided threads.

  if (nthread < 3 ){
    m_nthread <- nthread
    cm_nthread <- 1
  } else if (nthread < 15){
    m_nthread <- 2
    cm_nthread <- floor((nthread -1)/m_nthread)
  } else {
    m_nthread <- 4
    cm_nthread <- floor((nthread -1)/m_nthread)
  }

  logger::log_debug("Available cores: {nthread}, modified number of cores: {m_nthread}, cores to be used internally: {cm_nthread}")

  platform_os <- .Platform$OS.type

  if (is.element(platform_os,c("unix"))){
    matched_set <-  parallel::mclapply(bin_num,
                                       matching_fun,
                                       dataset=dataset[[1]],
                                       e_gps_pred = dataset[[2]],
                                       e_gps_std_pred = dataset[[3]],
                                       w_resid=dataset[[4]],
                                       gps_mx = gps_mx,
                                       w_mx = w_mx,
                                       delta_n = delta_n,
                                       scale = scale,
                                       nthread = cm_nthread,
                                       mc.cores = m_nthread)
  } else {
    matched_set <-  lapply(bin_num,
                           matching_fun,
                           dataset=dataset[[1]],
                           e_gps_pred = dataset[[2]],
                           e_gps_std_pred = dataset[[3]],
                           w_resid=dataset[[4]],
                           gps_mx = gps_mx,
                           w_mx = w_mx,
                           delta_n = delta_n,
                           scale = scale)
  }
  return(data.table(do.call(rbind,matched_set)))
}
