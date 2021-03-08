#' @title
#' Compute risk value
#'
#' @description
#' TODO
#'
#' @param h A scalar representing the bandwidth value.
#' @param matched_Y A vector of outcome variable in the matched set.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param w_vals A vector of values that you want to calculate the values of
#'  the ERF at.
#'
#' @return
#' TODO: return value
#' @keywords internal
#'
compute_risk <- function(h, matched_Y,matched_w,w_vals){
  hats <- estimate_hat_vals(h,matched_w,w_vals)
  tmp_mean <- mean(((matched_Y - smooth_erf(matched_Y,bw=h,matched_w = matched_w))/(1-hats))^2)
  return(tmp_mean)
}