#' @title
#' Estimate hat (fitted) values
#'
#' @description
#' Estimates the fitted values based on bandwidth value
#'
#' @param bw The bandwidth value.
#' @param matched.w A vector of continuous exposure variable in the matched set.
#' @param w.vals A vector of values that you want to calculate the values of the
#'  ERF at.
#'
#' @return
#' Returns fitted values, or the prediction made by the model for each observation.
#' @keywords internal
#'
EstimateHatvals <- function(bw,matched.w,w.vals){
  approx(w.vals,WFun(bw,matched.w,w.vals),xout=matched.w,rule=2)$y
}
