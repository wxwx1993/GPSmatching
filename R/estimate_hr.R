#' @title
#' Estimate hazard ratio
#'
#' @description
#' Estimates the hazard ratios using a parametric regression model.
#'
#' @param formula a vector of outcome variable in matched set.
#' @param family a description of the error distribution (see ?gam)
#' @param data dataset that formula is build upon
#'
#' @return
#' returns an object of class gnm
#'
#' @export
#'
estimate_hr<-function(formula, family, data){

  gnm_model <- gnm::gnm(formula = formula, family = family, data = data)
  return(gnm_model)
}
