#' @title
#' Compute minimum and maximum
#'
#' @description
#' Function to compute minimum and maximum of the input vector
#'
#' @param x vector
#'
#' @return
#' Returns a vector of length 2. The first element is min value, and the second
#' element is max value.
#'
#' @keywords internal
#'
ComputeMinMax <- function(x){

  min.x <- min(x,na.rm=T)
  max.x <- max(x,na.rm=T)

  return(c(min.x, max.x))
}
