#' @title
#' Estimate parametric coefficients
#'
#' @description
#' Estimates the parametric coefficients using a parametric regression model.
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam).
#' @param data dataset that formula is build upon.
#' @param ci_appr causal inference approach ("matching" or "weighting").
#'
#' @return
#' returns an object of class gnm
#'
#' @export
#'
estimate_pmetric_coefs<-function(formula, family, data, ci_appr){

  counter <- ipw <- NULL

  if (ci_appr == "matching"){
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
