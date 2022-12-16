#' @title
#' Check Kolmogorov-Smirnov (KS) statistics
#'
#' @description
#' Checks the Kolmogorov-Smirnov (KS) statistics for exposure and confounders in
#' the psuedo-population
#'
#' @param w A vector of observed continuous exposure variable.
#' @param c A data.frame of observed covariates variable.
#' @param ci_appr The causal inference approach.
#' @param optimized_compile If TRUE, use optimized compile approach.
#' @param counter_weight A weight vector in different situations. If the
#' matching approach is selected, it is an integer data.table of counters.
#' In the case of the weighting approach, it is weight data.table.
#' @param nthread The number of available threads.
#'
#' @return
#' output object is list including:
#'  - ks_stat
#'  - maximal_val
#'  - mean_val
#'  - median_val
#'
#' @keywords internal
#'
check_kolmogorov_smirnov <- function(w,
                                     c,
                                     ci_appr,
                                     optimized_compile,
                                     counter_weight = NULL,
                                     nthread=1){

  logger::log_debug("Started checking Kolmogorov-Smirnov (KS) statistics ... ")
  s_ks_t <- proc.time()

  data.table::setDF(w)
  data.table::setDF(c)
  data.table::setDF(counter_weight)
  tmp_data <- cbind(w, c)

  if (!(ci_appr %in% c("matching", "weighting"))){
    stop(paste (ci_appr, " is not a valid causal inference approach."))
  }

  name_vals <- names(tmp_data)

  if (optimized_compile){
    ks_stat <- lapply(name_vals,
                      function(i) {
                        Ecume::ks_test(x = as.numeric(tmp_data[[i]]),
                                       y = as.numeric(tmp_data[[i]]),
                                       w_x = rep(1, nrow(tmp_data)),
                                       w_y = counter_weight$counter_weight)$statistic})
    ks_stat <- unlist(ks_stat)
    names(ks_stat) <- name_vals
    stat_vals <- list(maximal_val = max(ks_stat, na.rm = TRUE),
                      mean_val = mean(ks_stat, na.rm = TRUE),
                      median_val = stats::median(ks_stat, na.rm = TRUE))

    output <- list(ks_stat = ks_stat,
                   stat_vals = stat_vals)

    # print the values into the info.
    logger::log_trace(paste0("Kolmogorov-Smirnov (KS): {paste(name_vals, ",
                             "ks_stat, collapse = ', ', sep = ' : ')}"))
    logger::log_trace(paste0("{paste(names(stat_vals), ",
                             "stat_vals, collapse = ', ', sep = ' : ')}"))

  } else {
    output <- NULL
  }



  e_ks_t <- proc.time()
  logger::log_debug("Finished KS (Wall clock time:  ",
                    " {(e_ks_t - s_ks_t)[[3]]} seconds).")

   return(output)
}
