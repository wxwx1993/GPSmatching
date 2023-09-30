#' @title
#' Estimate Parametric Exposure Response Function
#'
#' @description
#' Estimate a constant effect size for matched and weighted data set using
#' parametric models
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gnm)
#' @param data dataset that formula is build upon (Note that there should be a
#' `counter_weight` column in this data.)
#' @param ... Additional parameters for further fine tuning the gnm model.
#'
#' @details
#' This method uses generalized nonlinear model (gnm) from gnm package.
#'
#' @return
#' returns an object of class gnm
#'
#' @export
#'
#' @examples
#'\donttest{
#' m_d <- generate_syn_data(sample_size = 100)
#' pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
#'                                   m_d[, c("id", "cf1","cf2","cf3",
#'                                           "cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                   xgb_eta=c(0.1,0.2,0.3)),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   covar_bl_trs_type= "mean",
#'                                   max_attempt = 1,
#'                                   dist_measure = "l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#' data <- merge(m_d[, c("id", "Y")], pseudo_pop$pseudo_pop, by = "id")
#' outcome_m <- estimate_pmetric_erf(formula = Y ~ w,
#'                                   family = gaussian,
#'                                   data = data)
#'}
estimate_pmetric_erf <- function(formula, family, data, ...) {


  ## collect additional arguments
  dot_args <- list(...)
  named_args <- stats::setNames(dot_args, names(dot_args))

  if (any(data$counter_weight < 0)){
    stop("Negative weights are not allowed.")
  }

  if (sum(data$counter_weight) == 0) {
    data$counter_weight <- data$counter_weight + 1
    logger::log_debug("Giving equal weight for all samples.")
  }

  gnm_model <- do.call(gnm::gnm, c(list("formula" = formula,
                                        "family" = family,
                                        "data" = data,
                                        "weights" = data$counter_weight),
                                   named_args))

  if (is.null(gnm_model)) {
    stop("gnm model is null. Did not converge.")
  }

  return(gnm_model)
}
