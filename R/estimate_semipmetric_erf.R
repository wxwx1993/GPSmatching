#' @title
#' Estimate semi-exposure-response function (semi-ERF).
#'
#' @description
#' Estimates the smoothed exposure-response function using a generalized
#' additive model with splines.
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam).
#' @param data dataset that formula is build upon.
#' @param ... Additional parameters for further fine tuning the gam model.
#'
#' @details
#' This approach uses Generalized Additive Model (gam) using mgcv package.
#'
#' @return
#' returns an object of class gam
#'
#' @export
#'
#' @examples
#' m_d <- generate_syn_data(sample_size = 100)
#' pseudo_pop <- generate_pseudo_pop(m_d[, c("id", "Y")],
#'                                   m_d[, c("id", "w")],
#'                                   m_d[, c("id", "cf1","cf2","cf3","cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                    xgb_eta=c(0.1,0.2,0.3)),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   covar_bl_trs_type = "mean",
#'                                   max_attempt = 1,
#'                                   dist_measure = "l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
#' outcome_m <- estimate_semipmetric_erf (formula = Y ~ w,
#'                                        family = gaussian,
#'                                        data = pseudo_pop$pseudo_pop)
#'
#'
estimate_semipmetric_erf <- function(formula, family, data, ...) {


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

  gam_model <- do.call(gam::gam, c(list("formula" = formula,
                                        "family" = family,
                                        "data" = data,
                                        "weights" = data$counter_weight),
                                   named_args))

  if (is.null(gam_model)) {
    stop("gam model is null. Did not converge.")
  }

  return(gam_model)
}
