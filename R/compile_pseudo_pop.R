#' @title
#' Compile Pseudo Population
#'
#' @description
#' Compiles pseudo population based on the original population and estimated GPS
#' value.
#'
#' @param dataset List of size 6 including the following:
#'   - Original data set + GPS values (Y, w, GPS, counter, row_index, c)
#'   - e_gps_pred
#'   - e_gps_std_pred
#'   - w_resid
#'   - gps_mx (min and max of gps)
#'   - w_mx (min and max of w).
#' @param ci_appr Causal inference approach.
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric and non-parametric.
#' @param bin_seq Sequence of w (treatment) to generate pseudo population. If
#' NULL is passed the default value will be used, which is
#' `seq(min(w)+delta_n/2,max(w), by=delta_n)`.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
#' @param trim_quantiles A numerical vector of two. Represents the trim quantile
#' level. Both numbers should be in the range of \[0,1] and in increasing order
#' (default: c(0.01,0.99)).
#' @param optimized_compile If TRUE, uses counts to keep track of number of
#' replicated pseudo population.
#' @param ... Additional parameters.
#'
#' @note
#' The input data set should be output of estimate_gps function with
#' internal_use flag activated.
#'
#' @export
#'
#' @return
#' `compile_pseudo_pop` returns the pseudo population data that is compiled based
#' on the selected causal inference approach.
#'
#' @examples
#'
#' m_d <- generate_syn_data(sample_size = 100)
#' data_with_gps <- estimate_gps(m_d$Y,
#'                               m_d$treat,
#'                               m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                               pred_model = "sl",
#'                               gps_model = "parametric",
#'                               internal_use = TRUE,
#'                               params = list(xgb_max_depth = c(3,4,5),
#'                                        xgb_nrounds=c(10,20,30,40,50,60)),
#'                               nthread = 1,
#'                               sl_lib = c("m_xgboost")
#'                              )
#'
#'
#' pd <- compile_pseudo_pop(dataset = data_with_gps,
#'                          ci_appr = "matching",
#'                          gps_model = "parametric",
#'                          bin_seq = NULL,
#'                          nthread = 1,
#'                          trim_quantiles = c(0.01, 0.99),
#'                          optimized_compile=TRUE,
#'                          matching_fun = "matching_l1",
#'                          covar_bl_method = 'absolute',
#'                          covar_bl_trs = 0.1,
#'                          delta_n = 0.5,
#'                          scale = 1)
#'
compile_pseudo_pop <- function(dataset, ci_appr, gps_model = "parametric",
                               bin_seq = NULL, nthread = 1, trim_quantiles,
                               optimized_compile, ...){

  # Checking arguments
  check_args_compile_pseudo_pop(ci_appr, trim_quantiles=trim_quantiles,
                                optimized_compile=optimized_compile, ...)

  logger::log_info("Starting compiling pseudo population ",
                    " (original data size: {nrow(dataset[[1]])}) ... ")

  if (ci_appr == 'matching'){
      matched_set <- create_matching(dataset, bin_seq, gps_model, nthread,
                                     optimized_compile,...)
      logger::log_info("Finished compiling pseudo population ",
                      " (Pseudo population data size: {nrow(matched_set)})")
      return(matched_set)
  }

  if (ci_appr == 'weighting'){
    weighted_set <- create_weighting(dataset[[1]], ...)
    logger::log_info("Finished compiling pseudo population ",
                     " (Pseudo population data size: {nrow(weighted_set)})")
    return(weighted_set)
  }

  if (is.element(ci_appr, c('adjusting'))){
    stop(paste(ci_appr, " casual inference approach is not implemented."))
  }
  stop('The code should not get here. Something is wrong with checking arguments.')
}
