#' @title
#' Smooth exposure response function
#'
#' @description
#' Smooths exposure response function based on bandwidth
#'
#' @param matched.Y A vector of the outcome variable in the matched set.
#' @param bw The bandwidth value.
#' @param matched.w A vector of continuous exposure variable in the matched set.
#'
#' @return
#' Smoothed value of ERF
#' @keywords internal
#'
SmoothERF <- function(matched.Y,bw,matched.w){
  smoothed.val <- approx(locpoly(matched.w,matched.Y,bandwidth=bw, gridsize=1000),xout=matched.w,rule=2)$y
  return(smoothed.val)
}
