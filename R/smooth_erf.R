#' @title
#' Smooth exposure response function
#'
#' @description
#' Smooths exposure response function based on bandwidth
#'
#' @param matched_Y A vector of the outcome variable in the matched set.
#' @param bw The bandwidth value.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#'
#' @return
#' Smoothed value of ERF
#' @keywords internal
#'
smooth_erf <- function(matched_Y,bw,matched_w){

  if (length(bw)!=1){
    stop("bw should be of length 1.")
  }

  smoothed_val <- stats::approx(locpoly(matched_w, matched_Y, bandwidth=bw,
                                 gridsize=1000),
                         xout=matched_w,rule=2)$y
  return(smoothed_val)
}
