#' @title
#' Compute risk value
#'
#' @description
#' Calculates the cross-validated risk for the optimal bandwidth selection in
#' kernel smoothing approach.
#'
#' @param h A scalar representing the bandwidth value.
#' @param matched_Y A vector of outcome variable in the matched set.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param matched_cw A vector of counter or weight variable in the matched set.
#' @param w_vals A vector of values that you want to calculate the values of
#'  the ERF at.
#' @param kernel_appr Internal kernel approach. Available options are `locpol`
#' and `kernsmooth`.
#'
#' @return
#' returns a cross-validated risk value for the input bandwidth
#' @keywords internal
#'
compute_risk <- function(h,
                         matched_Y,
                         matched_w,
                         matched_cw,
                         x_eval,
                         w_vals,
                         kernel_appr) {

  hats <- estimate_hat_vals(h, matched_w, w_vals)
  tmp_mean <- mean(((matched_Y - smooth_erf(
                                    matched_Y = matched_Y,
                                    bw = h,
                                    matched_w = matched_w,
                                    matched_cw = matched_cw,
                                    x_eval = x_eval,
                                    kernel_appr = kernel_appr)) / (1 - hats)) ^ 2)
  return(tmp_mean)
}
