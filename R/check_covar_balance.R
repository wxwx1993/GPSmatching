#' @title
#' Check covariate balance
#'
#' @description
#' Checks the covariate balance of original population or pseudo population.
#'
#' @param w A vector of observed continuous exposure variable.
#' @param c A data.frame of observed covariates variable.
#' @param ci_appr The causal inference approach.
#' @param counter_weight A weight vector in different situations. If the
#' matching approach is selected, it is an integer data.table of counters.
#' In the case of the weighting approach, it is weight data.table.
#' @param nthread The number of available threads.
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
#'year <- sample(x=c("2001","2002","2003","2004","2005"),size = n,
#'               replace = TRUE)
#'region <- sample(x=c("North", "South", "East", "West"),size = n,
#'                 replace = TRUE)
#'mydata$year <- as.factor(year)
#'mydata$region <- as.factor(region)
#'mydata$cf5 <- as.factor(mydata$cf5)
#'
#'
#'
#'pseudo_pop <- generate_pseudo_pop(mydata[, c("id", "Y")],
#'                                  mydata[, c("id", "w")],
#'                                  mydata[, c("id", "cf1", "cf2", "cf3",
#'                                             "cf4","cf5", "cf6", "year",
#'                                             "region")],
#'                                  ci_appr = "matching",
#'                                  gps_density = "kernel",
#'                                  exposure_trim_qtls = c(0.01,0.99),
#'                                  sl_lib = c("m_xgboost"),
#'                                  covar_bl_method = "absolute",
#'                                  covar_bl_trs = 0.1,
#'                                  covar_bl_trs_type = "mean",
#'                                  max_attempt = 1,
#'                                  dist_measure = "l1",
#'                                  delta_n = 1,
#'                                  scale = 0.5,
#'                                  nthread = 1)
#'
#'adjusted_corr_obj <- check_covar_balance(w = pseudo_pop$pseudo_pop[, c("w")],
#'                                         c = pseudo_pop$pseudo_pop[ ,
#'                                         pseudo_pop$covariate_cols_name],
#'                                         counter = pseudo_pop$pseudo_pop[,
#'                                                     c("counter_weight")],
#'                                         ci_appr = "matching",
#'                                         nthread = 1,
#'                                         covar_bl_method = "absolute",
#'                                         covar_bl_trs = 0.1,
#'                                         covar_bl_trs_type = "mean")
#'

check_covar_balance <- function(w,
                                c,
                                ci_appr,
                                counter_weight = NULL,
                                nthread = 1,
                                ...){

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

  post_process_abs <- function(abs_cor) {

    covar_bl_t <- paste0(covar_bl_trs_type, "_absolute_corr")
    logger::log_debug(paste0(covar_bl_trs_type,
                            " absolute correlation: ",
                            getElement(abs_cor, covar_bl_t)))
    message(paste0(covar_bl_trs_type, " absolute correlation: ",
                  getElement(abs_cor, covar_bl_t),
                  "| Covariate balance threshold: ", covar_bl_trs))

    output <- list(corr_results = abs_cor)


    if (getElement(abs_cor,covar_bl_t) < covar_bl_trs) {
      output$pass <- TRUE
    } else {
      output$pass <- FALSE
    }

    e_ccb_t <- proc.time()
    logger::log_debug("Finished checking covariate balance (Wall clock time:  ",
                      " {(e_ccb_t - s_ccb_t)[[3]]} seconds).")
    return(output)

  }

  if (covar_bl_method != "absolute") {
    stop(paste(covar_bl_method, " method for covariate balance is not a valid
               option or not implemented."))
  }

  if (!(ci_appr %in% c("matching", "weighting"))) {
    stop(paste(ci_appr, " is not a valid causal inference approach."))
  }

  if (is.null(counter_weight)) {
    abs_cor <- absolute_corr_fun(w, c)
    return(post_process_abs(abs_cor))
  }

  if (ci_appr == "matching"){
      abs_cor <- absolute_weighted_corr_fun(w = w,
                                            vw = counter_weight,
                                            c = c)

      return(post_process_abs(abs_cor))
  }

  if (ci_appr == "weighting"){
    abs_cor <- absolute_weighted_corr_fun(w = w,
                                          vw = counter_weight,
                                          c = c)
    return(post_process_abs(abs_cor))
  }

  stop(paste0("Input values for check_covar_balance are not correct.",
       " The code should not get here. Please inform the developers."))
}
