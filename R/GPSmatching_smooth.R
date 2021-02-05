#' @title
#' Generate Kernel function
#'
#' @description
#' TODO
#'
#' @param t standardized vector (z-score)
#'
#' @return
#' probability distribution
#'
#' @keywords internal
#'
#' @importFrom stats dnorm
#'
GenerateKernel <- function(t){
  dnorm(t)
}

#' TODO: Function title
#'
#' @param bw bandwidth value
#' @param matched.w a vector of continuous exposure variable in matched set.
#' @param w.vals a vector of values that you want to calculate the values of the ERF at.
#'
#' @return
#' return value (TODO)
#' @keywords internal
#'
WFun <- function(bw,matched.w,w.vals){
  w.avals <- NULL
  for (w.val in w.vals){
    w.std <- (matched.w-w.val)/bw
    kern.std <- GenerateKernel(w.std)/bw
    tmp.mean <- mean(w.std^2*kern.std)
    w.avals <- c(w.avals, tmp.mean*(GenerateKernel(0)/bw) /
                   (mean(kern.std)*tmp.mean-mean(w.std*kern.std)^2))
  }
  return(w.avals/length(matched.w))
}



