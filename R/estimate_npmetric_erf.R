#' @title
#' Estimate Smoothed Exposure-Response Function (ERF) for Matched Data Set.
#'
#' @description
#' Estimate smoothed exposure-response function (ERF) for matched and weighted
#' data set using non-parametric models.
#'
#' @param matched_Y a vector of outcome variable in the matched set.
#' @param matched_w a vector of continuous exposure variable in the matched set.
#' @param matched_counter a vector of counter variable in the matched set.
#' @param bw_seq a vector of bandwidth values (Default is seq(0.2,2,0.2)).
#' @param w_vals a vector of values that you want to calculate the values of
#'  the ERF at.
#' @param nthread number of available cores.
#'
#' @details
#' Estimate Functions Using Local Polynomial kernel regression Package: ‘KernSmooth’.
#'
#' @return
#' The function returns a gpsm_erf object. The object includes the following
#' attributes:
#'
#' - params
#'  - matched_Y
#'  - matched_w
#'  - bw_seq
#'  - w_vals
#' - erf
#' - fcall
#'
#' @export
#'
#' @examples
#'
#' m_d <- generate_syn_data(sample_size = 100)
#' pseudo_pop <- generate_pseudo_pop(m_d$Y,
#'                                   m_d$treat,
#'                                   m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   pred_model = "sl",
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                    xgb_eta=c(0.1,0.2,0.3)),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   max_attempt = 1,
#'                                   matching_fun = "matching_l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
#' erf_obj <- estimate_npmetric_erf(pseudo_pop$pseudo_pop$Y,
#'                                  pseudo_pop$pseudo_pop$w,
#'                                  bw_seq=seq(0.2,2,0.2),
#'                                  w_vals = seq(2,20,0.5),
#'                                  nthread = 1)
#'
estimate_npmetric_erf<-function(matched_Y,
                                matched_w,
                                matched_counter = NULL,
                                bw_seq=seq(0.2,2,0.2),
                                w_vals,
                                nthread){

  # function call
  fcall <- match.call()

  if (length(matched_Y) != length(matched_w)){
    stop("Length of output and treatment should be equal!")
  }

  if (!is.double(matched_Y) || !is.double(matched_w)){
    stop("Output and treatment vectors should be double vectors.")
  }

  if (!is.null(matched_counter)){
    if (length(matched_Y) != length(matched_counter)){
      stop("Length of matched_counter should be according to other inputs.")
    } else {
      matched_Y <- tidyr::uncount(data.frame(matched_Y), matched_counter)[,1]
      matched_w <- tidyr::uncount(data.frame(matched_w), matched_counter)[,1]
    }
  }

  cl <- parallel::makeCluster(nthread, type="PSOCK",
                              outfile="CausalGPS.log")

  risk_val_1 <-  parallel::parLapply(cl,
                                     bw_seq,
                                     compute_risk,
                                     matched_Y = matched_Y,
                                     matched_w = matched_w,
                                     w_vals = w_vals)

  parallel::stopCluster(cl)

  risk_val <- do.call(rbind, risk_val_1)[,1]

  h_opt <- bw_seq[which.min(risk_val)]
  erf <- stats::approx(locpoly(matched_w, matched_Y, bandwidth=h_opt), xout=w_vals)$y

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
