#' @title
#' Estimate Parametric Exposure Response Function
#'
#' @description
#' Estimate a constant effect size for matched and weighted data set using
#' parametric models
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gnm)
#' @param data dataset that formula is build upon
#' @param ci_appr causal inference approach (matching or weighting).
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
#'
#' m_d <- generate_syn_data(sample_size = 100)
#' pseudo_pop <- generate_pseudo_pop(m_d$Y,
#'                                   m_d$treat,
#'                                   m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   pred_model = "sl",
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                    xgb_eta=c(0.1,0.2,0.3)),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   covar_bl_trs_type= "mean",
#'                                   max_attempt = 1,
#'                                   matching_fun = "matching_l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
#'  outcome_m <- estimate_pmetric_erf(formula = Y ~ w,
#'                                     family = gaussian,
#'                                     data = pseudo_pop$pseudo_pop,
#'                                     ci_appr = "matching")
#'
estimate_pmetric_erf <- function(formula, family, data, ci_appr){

  counter <- ipw <- NULL

  if (ci_appr == "matching"){

    # If the approach is not optimized, the counter will be zero, which causes
    # problem in generating prediction model.
    if (sum(data$counter) == 0) {
      data$counter <- data$counter + 1
      logger::log_debug("Giving equal weight for all samples.")
    }

    suppressWarnings(gnm_model <- gnm::gnm(formula = formula,
                                           family = family,
                                           data = data,
                                           weights = counter,
                                           verbose = FALSE, model = FALSE))
  } else if (ci_appr == "weighting"){
    suppressWarnings(gnm_model <- gnm::gnm(formula = formula,
                                           family = family,
                                           data = data,
                                           weights = ipw,
                                           verbose = FALSE, model = FALSE))
  } else {
    stop(paste("ci_appr: ", ci_appr, " is not a valid causal inference."))
  }

  if (is.null(gnm_model)) {
    stop("gnm model is null. Did not converge.")
  }

  return(gnm_model)
}
