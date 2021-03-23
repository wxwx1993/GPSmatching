#' @title
#' Check covariate balance
#'
#' @description
#' Checks the covariate balance of
#'
#' @param pseudo_pop The generated pseudo population. In the following format:
#'   - 1st column: outcome (Y)
#'   - 2nd column: exposure (w)
#'   - 3rd column: gps
#'   - 4th column to the end: covariates (c)
#' @param ci_appr The causal inference approach.
#' @param nthread The number of available threads.
#' @param ... Additional arguments passed to different models.
#'
#' @keywords internal
#'
#' @return
#' Returns True if the pseudo population meet the covariate balance test
#'  requirements.
#' @export
#'
check_covar_balance <- function(pseudo_pop, ci_appr, nthread=1, ...){

  # Passing packaging check() ----------------------------
  covar_bl_method <- NULL
  covar_bl_trs <- NULL
  # ------------------------------------------------------

  # collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i], use.names = FALSE))
  }

  if (ci_appr == 'adjust'){
    # No covariate balance test for the 'adjust' causal inference approach.
    stop("The code should never get here. Argument checks or while loop checks
         are not correct.")
  }

  if (covar_bl_method == 'absolute'){
    if (ci_appr == 'matching'){
      abs_cor <- absolute_corr_fun(pseudo_pop[, 2],
                                   pseudo_pop[,4:length(pseudo_pop)], nthread)
    } else if (ci_appr == 'weighting') {
      abs_cor <- absolute_weighted_corr_fun(pseudo_pop[, 2],pseudo_pop[, 4],
                                            pseudo_pop[, 5:length(pseudo_pop)])
    } else {
      stop(paste("Selected causal inference approach (ci_appr =", ci_appr,
                 ") is not implemented."))
    }

    message(paste("Mean absolute correlation: ", abs_cor$mean_absolute_corr,
                  "| Covariate balance threshold: ", covar_bl_trs))


    output <- list(corr_results = abs_cor)
    if (abs_cor$mean_absolute_corr < covar_bl_trs){
      output$pass <- TRUE
    } else {
      output$pass <- FALSE
    }
      return(output)
  } else {
    stop(paste(covar_bl_method, " method for covariate balance is not a valid
               option."))
  }
}
