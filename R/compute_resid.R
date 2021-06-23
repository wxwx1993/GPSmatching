#'@title
#'Compute Residual
#'
#'@description
#' Function to compute residual
#'
#' @param a description
#' @param b description
#' @param c description
#'
#' @return
#' return value (TODO)
#'
#' @keywords internal
#'
compute_resid <- function(a, b, c){

  val_1 <- mapply('-',a,b)
  val_2 <- mapply('/',val_1,c)
  return(val_2)
}