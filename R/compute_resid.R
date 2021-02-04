
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

  val1 <- mapply('-',a,b)
  val2 <- mapply('/',val1,c)
  return(val2)

}
