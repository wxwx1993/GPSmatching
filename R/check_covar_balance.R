#' @title
#' Check covariate balance
#'
#' @description
#' Checks the covariate balance of
#'
#' @param pseudo.pop The generated pseudo population. In the following format:
#'   - 1st column: outcome (Y)
#'   - 2nd column: exposure (w)
#'   - 3rd column: gps
#'   - 4th column to the end: covariates (c)
#' @param ci.appr The causal inference approach.
#' @param ... Additional arguments passed to different models.
#'
#' @keywords internal
#'
#' @return
#' Returns True if the pseudo population meet the covariate balance test
#'  requirements.
#' @export
#'
CheckCovarBalance <- function(pseudo.pop, ci.appr, ...){

  # Passing packaging check() ----------------------------
  covar.bl.method <- NULL
  covar.bl.trs <- NULL
  # ------------------------------------------------------

  # collect additional arguments
  dot.args <- list(...)
  arg.names <- names(dot.args)

  for (i in arg.names){
    assign(i,unlist(dot.args[i],use.names = FALSE))
  }

  if (ci.appr == 'adjust'){
    # No covariate balance test for the 'adjust' causal inference approach.
    stop("The code should never get here. Argument checks or while loop checks
         are not correct.")
  }

  if (covar.bl.method == 'absolute'){
    abs.cor <- AbsoluteCorrFun(pseudo.pop[,2], pseudo.pop[,4:length(pseudo.pop)])

    if (abs.cor$mean_absolute.corr < covar.bl.trs){
      message(paste("Mean absolute correlation: ", abs.cor$mean_absolute.corr,
                    "| Covariate balance threshold: ", covar.bl.trs))
      return(TRUE)
    } else {
      return(FALSE)
    }

  } else {
    stop(paste(covar.bl.method, " method for covariate balance is not a valid
               option."))
  }
}
