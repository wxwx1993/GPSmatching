#' @title
#' Extend print function for gpsm_erf object
#'
#' @param x A gpsm_erf object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.gpsm_erf <- function(x, ...){

  x <- unclass(x)

  cat(" CausalGPS exposure rate function object\n")
  cat(" function call: \n")
  cat("      ***       \n")
  print(x$fcall, ...)
  cat("      ***       \n")
  cat(" Output data can be accessed at $erf \n")
  cat(" Look at summary for more details.")
}


#' @title
#' print summary of gpsm_erf object

#'
#' @param object A gpsm_erf object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' Returns summary of data
#' @export
summary.gpsm_erf <- function(object, ...){

  cat_list <- function(input){
    cat(paste("   size: ", length(input),
              ", class: ", class(input),
              ", missing value(s): ", sum(is.na(input)),
              sep = ""))
    if (is.numeric(input)){
      cat(paste("\n   min: ", sprintf("%.3f", min(input, na.rm = TRUE)),
                "\n   max: ", sprintf("%.3f", max(input, na.rm = TRUE)),
                "\n   mean: ", sprintf("%.3f", mean(input, na.rm = TRUE)),
                sep = ""))
    }
  }

  object <- unclass(object)
  cat("Input data: \n")
  for (item in names(object$params)){
    cat(paste(" ", item, "\n"))
    cat_list(object$params[[item]])
    cat("\n")
  }
  cat("\nOutput data: \n")
  cat(paste("  erf\n"))
  cat_list(object$erf)
}


#' @title
#' Extend print function for gpsm_pspop object
#'
#' @param x A gpsm_pspop object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @export
#'
print.gpsm_pspop <- function(x, ...){

  x <- unclass(x)

  cat(" CausalGPS pseudo population object\n")
  cat(" function call: \n")
  cat("      ***       \n")
  print(x$fcall, ...)
  cat("      ***       \n")
  cat(" Output data can be accessed at $pseudo_pop \n")
  cat(" Look at summary for more details.")
}


#' @title
#' print summary of gpsm_pspop object

#'
#' @param object A gpsm_pspop object.
#' @param ... Additional arguments passed to customize the results.
#'
#' @return
#' Returns summary of data
#' @export
summary.gpsm_pspop <- function(object, ...){

  cat("CausalGPS pseudo population object summary\n")
  cat_list <- function(input){
    cat(paste("   size: ", length(input),
              ", class: ", class(input),
              ", missing value(s): ", sum(is.na(input)),
              sep = ""))
    if (is.numeric(input)){
      cat(paste("\n   min: ", sprintf("%.3f", min(input, na.rm = TRUE)),
                "\n   max: ", sprintf("%.3f", max(input, na.rm = TRUE)),
                "\n   mean: ", sprintf("%.3f", mean(input, na.rm = TRUE)),
                sep = ""))
    }
  }

  object <- unclass(object)
  for (item in names(object$pseudo_pop)){
    cat(paste(" ", item, "\n"))
    cat_list(object$pseudo_pop[[item]])
    cat("\n")
  }
}
