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
print.gpsm_erf <- function(x, ...) {

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
summary.gpsm_erf <- function(object, ...) {

  cat_list <- function(input) {
    cat(paste("   size: ", length(input),
              ", class: ", class(input),
              ", missing value(s): ", sum(is.na(input)),
              sep = ""))
    if (is.numeric(input)) {
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
print.gpsm_pspop <- function(x, ...) {

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
summary.gpsm_pspop <- function(object, ...) {

  cat("--- CausalGPS pseudo population object summary --- \n")
  cat(paste("Pseudo population met the covariate balance requirement: ",
            object$passed_covar_test, "\n"))
  cat(paste("Absolute correlation of the original data: \n",
            "  mean:    ", sprintf("%.3f",
                            object$original_corr_results$mean_absolute_corr),
            "\n",
            "  median:  ", sprintf("%.3f",
                            object$original_corr_results$median_absolute_corr),
            "\n",
            "  maximal: ", sprintf("%.3f",
                            object$original_corr_results$maximal_absolute_corr),
            "\n"
            ))
  cat(paste("\n", names(object$original_corr_results$absolute_corr), ":",
            sprintf("%.3f",object$original_corr_results$absolute_corr)))
  cat(paste("\n\n Absolute correlation of the pseudo population: \n",
            "  mean:    ", sprintf("%.3f",
                            object$adjusted_corr_results$mean_absolute_corr),
            "\n",
            "  median:  ", sprintf("%.3f",
                            object$adjusted_corr_results$median_absolute_corr),
            "\n",
            "  maximal: ", sprintf("%.3f",
                            object$adjusted_corr_results$maximal_absolute_corr),
            "\n"
  ))
  cat(paste("\n", names(object$adjusted_corr_results$absolute_corr), ":",
            sprintf("%.3f",object$adjusted_corr_results$absolute_corr)))
  cat(paste("\n\n Hyperparameters used for the select population:"))
  cat(paste("\n", names(object$best_gps_used_params), ":",
            object$best_gps_used_params))
  cat("\n\n")
  cat(paste("Number of data samples: ", nrow(object$pseudo_pop), "\n"))
  cat(paste("Number of iterations: ", object$counter, "\n"))
  cat("Effective sample size: \n")
  cat(paste("  Achieved: ", object$ess, "\n"))
  cat(paste("  Min recommended: ", object$ess_recommended, "\n"))
  cat("Kolmogorov-Smirnov (KS) statistics:")
  if (is.null(object$ks_stats)){
    cat("\n  Not computed. \n")
  } else {
    cat(paste("\n", " ", names(object$ks_stats$ks_stat), ":",
              sprintf("%.3f", object$ks_stats$ks_stat)))
    cat(paste("\n summary: \n",
              "  mean:    ",
              sprintf("%.3f", object$ks_stats$stat_vals[["mean_val"]]), "\n",
              "  median:  ",
              sprintf("%.3f", object$ks_stats$stat_vals[["median_val"]]), "\n",
              "  maximal: ",
              sprintf("%.3f", object$ks_stats$stat_vals[["maximal_val"]]), "\n"
    ))
  }
  cat("--- *** --- \n")
}
