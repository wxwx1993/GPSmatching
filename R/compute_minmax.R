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
compute_min_max <- function(x) {

  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)

  return(c(min_x, max_x))
}
