#' @title
#' Estimate semi exposure-response function (semi-ERF).
#'
#' @description
#' Estimates the smoothed exposure-response function using a generalized
#' additive model with splines.
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam)
#' @param data dataset that formula is build upon
#'
#' @return
#' returns an object of class gam
#'
#' @export
#'
estimate_semi_erf<-function(formula, family, data){

    gam_model <- gam::gam(formula = formula, family = family, data = data)

  return(gam_model)
}
