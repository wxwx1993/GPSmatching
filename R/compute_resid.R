#' @title
#' Compute Residual
#'
#' @description
#' Function to compute residual
#'
#' @param a A vector
#' @param b A vector
#' @param c A vector
#'
#' @return
#' returns a residual values.
#'
#' @keywords internal
#'
compute_resid <- function(a, b, c){

  val_1 <- mapply('-',a,b)
  val_2 <- mapply('/',val_1,c)
  return(val_2)
}
