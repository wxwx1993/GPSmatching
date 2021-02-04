#' A function to find the closest data in subset to the original data
#'
#' Function description (TODO)
#'
#' @param a  parameter description (TODO)
#' @param b  parameter description
#' @param c  parameter description
#' @param d  parameter description
#' @param sc parameter description
#'
#' @return
#' The function returns index of subset data that is closest to the original data
#' sample.
#'
#' @keywords internal
#'
ComputeClosestWGPS <- function(a,b,c,d,sc){

  if (!is.numeric(a) ||
      !is.numeric(b) ||
      !is.numeric(c) ||
      !is.numeric(d) ||
      !is.numeric(sc)){
    stop('Input values for compute_closest_wgps should be numeric.')
  }

  if (length(d) != 1){
    stop('Expecting a scaler number for d.')
  }


  if (length(sc) != 1){
    stop('Expecting a scaler number for sc(scale).')
  }

  if (length(a) != length(c)){
    stop('Expecting equal length for a and c.')
  }

  wm <- apply(ComputeOuter(a, b, '-') * sc,
              2,
              function(x) which.min(abs(c - d) * (1 - sc) + x)
  )
}
