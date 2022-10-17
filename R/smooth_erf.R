#' @title
#' Smooth exposure response function
#'
#' @description
#' Smooths exposure response function based on bandwidth
#'
#' @param matched_Y A vector of the outcome variable in the matched set.
#' @param bw The bandwidth value.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param matched_cw A vector of counter or weight variable in the matched set.
#'
#' @return
#' Smoothed value of ERF
#' @keywords internal
#'
smooth_erf <- function(matched_Y,bw,matched_w, matched_cw){

  if (length(bw)!=1){
    stop("bw should be of length 1.")
  }

  if (sum(matched_cw == 0) == length(matched_cw)) {
    matched_cw <- matched_cw + 1
    logger::log_debug("Giving equal weight for all samples.")
  }

  data <- data.frame(matched_Y = matched_Y, matched_w = matched_w)
  val <- locpol::locpol(formula = matched_Y~matched_w,
                        data = data,
                        bw = bw,
                        weig = matched_cw,
                        xeval = matched_w,
                        kernel = locpol::gaussK)

  smoothed_val <- val$lpFit$matched_Y

  return(smoothed_val)
}
