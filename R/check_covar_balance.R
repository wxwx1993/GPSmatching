#' @title
#' Check Covariate Balance
#'
#' @description
#' Checks the covariate balance of original population or pseudo population.
#'
#' @param pseudo_pop The generated pseudo population. In the following format:
#'   - 1st column: outcome (Y)
#'   - 2nd column: exposure (w)
#'   - 3rd column: gps
#'   - 4th column to the end: covariates (c)
#' @param ci_appr The causal inference approach.
#' @param nthread The number of available threads.
#' @param optimized_compile If TRUE, use optimized compile approach.
#' @param ... Additional arguments passed to different models.
#'
#' @details
#' ## Additional parameters
#'   - For ci_appr == matching:
#'     - covar_bl_method
#'     - covar_bl_trs
#'
#' @return
#' output object:
#'  - corr_results
#'    - absolute_corr
#'    - mean_absolute_corr
#'  - pass (TRUE,FALSE)
#'
#' @export
#'
#' @examples
#' set.seed(422)
#' n <- 100
#'mydata <- generate_syn_data(sample_size=100)
#'year <- sample(x=c("2001","2002","2003","2004","2005"),size = n, replace = TRUE)
#'region <- sample(x=c("North", "South", "East", "West"),size = n, replace = TRUE)
#'mydata$year <- as.factor(year)
#'mydata$region <- as.factor(region)
#'mydata$cf5 <- as.factor(mydata$cf5)
#'
#'pseudo_pop <- generate_pseudo_pop(mydata$Y,
#'                                  mydata$treat,
#'                                  mydata[c("cf1","cf2","cf3","cf4","cf5","cf6","year","region")],
#'                                  ci_appr = "matching",
#'                                  pred_model = "sl",
#'                                  gps_model = "non-parametric",
#'                                  trim_quantiles = c(0.01,0.99),
#'                                  optimized_compile = TRUE,
#'                                  sl_lib = c("m_xgboost"),
#'                                  covar_bl_method = "absolute",
#'                                  covar_bl_trs = 0.1,
#'                                  covar_bl_trs_type = "mean",
#'                                  max_attempt = 1,
#'                                  matching_fun = "matching_l1",
#'                                  delta_n = 1,
#'                                  scale = 0.5,
#'                                  nthread = 1)
#'
#'adjusted_corr_obj <- check_covar_balance(pseudo_pop$pseudo_pop,
#'                                         ci_appr="matching",
#'                                         nthread=1,
#'                                         covar_bl_method = "absolute",
#'                                         covar_bl_trs = 0.1,
#'                                         covar_bl_trs_type = "mean",
#'                                         optimized_compile=FALSE)
#'

check_covar_balance <- function(pseudo_pop, ci_appr, nthread=1,
                                optimized_compile, ...){

  # Passing packaging check() ----------------------------
  covar_bl_method <- NULL
  covar_bl_trs <- NULL
  covar_bl_trs_type <- NULL
  # ------------------------------------------------------

  logger::log_debug("Started checking covariate balance ... ")
  s_ccb_t <- proc.time()

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
      if (!optimized_compile){
        abs_cor <- absolute_corr_fun(pseudo_pop[, 2],
                                     pseudo_pop[,6:length(pseudo_pop)])
        #names(abs_cor$absolute_corr) <- names(pseudo_pop)[6:length(pseudo_pop)]
      } else if (optimized_compile){
        abs_cor <- absolute_weighted_corr_fun(pseudo_pop[, 2], pseudo_pop[, 4],
                                     pseudo_pop[,6:length(pseudo_pop)])
        #names(abs_cor$absolute_corr) <- names(pseudo_pop)[6:length(pseudo_pop)]
      } else {
        stop("The code should never get here. There is something wrong with check arguments.")
      }
    } else if (ci_appr == 'weighting') {
      abs_cor <- absolute_weighted_corr_fun(pseudo_pop[, 2],pseudo_pop[, 6],
                                            pseudo_pop[, 7:length(pseudo_pop)])
      #names(abs_cor$absolute_corr) <- names(pseudo_pop)[7:length(pseudo_pop)]
    } else {
      stop(paste("Selected causal inference approach (ci_appr =", ci_appr,
                 ") is not implemented."))
    }

    logger::log_debug(paste("Mean absolute correlation: ",
                            abs_cor$mean_absolute_corr))
    message(paste("Mean absolute correlation: ", abs_cor$mean_absolute_corr,
                  "| Covariate balance threshold: ", covar_bl_trs))

    output <- list(corr_results = abs_cor)
    covar_bl_t <- paste0(covar_bl_trs_type,"_absolute_corr")

    if (getElement(abs_cor,covar_bl_t) < covar_bl_trs){
      output$pass <- TRUE
    } else {
      output$pass <- FALSE
    }

    e_ccb_t <- proc.time()
    logger::log_debug("Finished checking covariate balance (Wall clock time:  ",
                      " {(e_ccb_t - s_ccb_t)[[3]]} seconds).")
    return(output)
  } else {
    stop(paste(covar_bl_method, " method for covariate balance is not a valid
               option."))
  }
}
