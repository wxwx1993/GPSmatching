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
ComputeResid <- function(a, b, c){

  val.1 <- mapply('-',a,b)
  val.2 <- mapply('/',val.1,c)
  return(val.2)
}
