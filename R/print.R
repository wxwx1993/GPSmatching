
#' @title
#' Extend print function for gpsm_erf object
#'
#' @param object A gpsm_erf object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' NULL
#' @export
#'
print.gpsm_erf <- function(object, ...){
  cat("----------- GPSmatching exposure rate function ---------------\n")
  cat(" function call: \n")
  cat("      ***       \n")
  print(object$fcall)
  cat("      ***       \n")
  cat(" Output data can be accessed at $erf \n")
  cat(" Look at summary for more details.")
}


#' Title
#'
#' @param object A gpsm_erf object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' Returns summary of data
#' @export
#'
summary.gpsm_erf <- function(object, ...){
  cat("Summary of Input data: \n")
  for (item in names(object$params)){
     print.data.frame(object$params[item])
  }
}
