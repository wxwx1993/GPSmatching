#' @title
#' Estimate smoothed exposure-response function (ERF).
#'
#' @description
#' TODO
#'
#' @param matched_Y a vector of outcome variable in matched set.
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param bw_seq a vector of bandwidth values (Default is seq(0.2,2,0.2)).
#' @param w_vals a vector of values that you want to calculate the values of
#'  the ERF at.
#'
#' @return
#' \code{erf}: The function returns a vector saved the output values of
#'  exposure-response function (ERF) given input \code{w.vals}.
#'
#' @export
#'
#' @examples
#'
#' m_d <- gen_syn_data(sample_size = 100)
#'
#'
#'
estimate_erf<-function(matched_Y,
                       matched_w,
                       bw_seq=seq(0.2,2,0.2),
                       w_vals){
  risk_val <- sapply(bw_seq, compute_risk, matched_Y = matched_Y,
                     matched_w = matched_w, w_vals = w_vals)
  h_opt <- bw_seq[which.min(risk_val)]
  erf <- approx(locpoly(matched_w, matched_Y, bandwidth=h_opt), xout=w_vals)$y
  return(erf)
}
