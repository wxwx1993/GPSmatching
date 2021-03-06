#' @title
#' Compile pseudo population
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
#' @param ... Additional parameters.
#'
#' @keywords internal
#'
#' @return
#' `compile_pseudo_pop` returns the pseudo population data that is compiled based
#' on selected causal inference approach.
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
