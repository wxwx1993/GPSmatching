#' @title
#' Estimate hat (fitted) values
#'
#' @description
#' Estimates the fitted values based on bandwidth value
#'
#' @param bw The bandwidth value.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param w_vals A vector of values that you want to calculate the values of the
#'  ERF at.
#'
#' @return
#' Returns fitted values, or the prediction made by the model for each observation.
#' @keywords internal
#'
estimate_hat_vals <- function(bw,matched_w,w_vals){
  stats::approx(w_vals,w_fun(bw,matched_w,w_vals),xout=matched_w,rule=2)$y
}
