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
#' pseudo_pop <- gen_pseudo_pop(m_d$Y,
#'                              m_d$treat,
#'                              m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                              ci_appr = "matching",
#'                              running_appr = "base",
#'                              pred_model = "sl",
#'                              sl_lib = c("m_xgboost"),
#'                              params = list(xgb_nrounds=c(10,20,30),
#'                               xgb_eta=c(0.1,0.2,0.3)),
#'                              nthread = 1,
#'                              covar_bl_method = "absolute",
#'                              covar_bl_trs = 0.1,
#'                              max_attempt = 1,
#'                              matching_fun = "matching_l1",
#'                              delta_n = 1,
#'                              scale = 0.5)
#'
#' erf_val <- estimate_erf(pseudo_pop$Y,
#'                         pseudo_pop$w,
#'                         bw_seq=seq(0.2,2,0.2),
#'                         w_vals = seq(2,20,0.5))
#'
#'
estimate_erf<-function(matched_Y,
                       matched_w,
                       bw_seq=seq(0.2,2,0.2),
                       w_vals){

  # function call
  fcall <- match.call()

  risk_val <- sapply(bw_seq, compute_risk, matched_Y = matched_Y,
                     matched_w = matched_w, w_vals = w_vals)
  h_opt <- bw_seq[which.min(risk_val)]
  erf <- approx(locpoly(matched_w, matched_Y, bandwidth=h_opt), xout=w_vals)$y

  result <- list()
  class(result) <- "gpsm_erf"
  result$params$matched_Y <- matched_Y
  result$params$matched_w <- matched_w
  result$params$bw_seq <- bw_seq
  result$params$w_vals <- w_vals
  result$erf <- erf
  result$fcall <- fcall

  return(result)
}
