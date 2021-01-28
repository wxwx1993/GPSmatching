#' A function to impute missing values based on density estimation.
#'
#'
#' @param x0 description of x0
#' @param x1 desctiption of x1
#'
#' @return
#' return value (TODO)
#'
#' @keywords internal
#'
compute_density <- function(x0, x1){
  tmp_density <- density(x0,na.rm = TRUE)
  approx(tmp_density$x,tmp_density$y,xout=x1,rule=2)$y
}
