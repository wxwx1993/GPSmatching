#' @title
#' Compute risk value
#'
#' @description
#' TODO
#'
#' @param h A scalar representing the bandwidth value.
#' @param matched.Y A vector of outcome variable in the matched set.
#' @param matched.w A vector of continuous exposure variable in the matched set.
#' @param w.vals A vector of values that you want to calculate the values of
#'  the ERF at.
#'
#' @return
#' TODO: return value
#' @keywords internal
#'
ComputeRisk <- function(h, matched.Y,matched.w,w.vals){
  hats <- EstimateHatvals(h,matched.w,w.vals)
  tmp.mean <- mean( ((matched.Y - SmoothERF(matched.Y,bw=h,matched.w = matched.w))/(1-hats))^2)
  return(tmp.mean)
}
