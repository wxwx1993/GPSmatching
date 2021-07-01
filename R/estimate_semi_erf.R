#' @title
#' Estimate semi exposure-response function (semi-ERF).
#'
#' @description
#' Estimates the smoothed exposure-response function using a generalized
#' additive model with splines.
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam).
#' @param data dataset that formula is build upon.
#' @param ci_appr causal inference approach (matching or weighting).
#'
#' @return
#' returns an object of class gam
#'
#' @export
#'
estimate_semi_erf<-function(formula, family, data, ci_appr){

  counter <- ipw <- NULL

  if (ci_appr == "matching"){
    suppressWarnings(gam_model <- gam::gam(formula = formula,
                                           family = family,
                                           data = data,
                                           weights = counter))
  } else if (ci_appr == "weighting"){
    suppressWarnings(gam_model <- gam::gam(formula = formula,
                                           family = family,
                                           data = data,
                                           weights = ipw))
  } else {
    stop(paste("ci_appr: ", ci_appr, " is not a valid causal inference."))
  }


  if (is.null(gam_model)) {
    stop("gam model is null. Did not converge.")
  }

  return(gam_model)
}
