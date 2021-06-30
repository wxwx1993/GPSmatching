#' @title
#' Approximate density based on another vector
#'
#' @description
#' A function to impute missing values based on density estimation of another
#' vector or itself after removing the missing values.
#'
#' @param x0 vector
#' @param x1 vector
#'
#' @return
#' Returns approximation of density value of vector x1 based on vector x0.
#'
#' @keywords internal
#'
compute_density <- function(x0, x1){
  tmp_density <- stats::density(x0, na.rm = TRUE)
  dnsty <- stats::approx(tmp_density$x, tmp_density$y, xout=x1, rule=2)$y
  return(dnsty)
}
