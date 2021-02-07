#' @title
#' Compile pseudo population
#'
#' @description
#' Copiles pseudo population based on the original population and estimated GPS
#' value.
#'
#' @param dataset List of size 6 including the following:
#'   - Original data set + GPS values (Y, w, GPS, c)
#'   - e_gps_pred
#'   - e_gps_std_pred
#'   - w_resid
#'   - gps_mx (min and max of gps)
#'   - w_mx (min and max of w).
#' @param ci.appr Causal inference approach.
#' @param ... Additional parameters.
#'
#' @keywords internal
#'
#' @return
#' `CompilePseudoPop` returns the pseudo population data that is compiled based
#' on selected causal inference approach.
#' @export
#'
CompilePseudoPop <- function(dataset, ci.appr, ...){

  # Checking arguments
  CheckArgsCPseudoPop(ci.appr, ...)

  if (ci.appr == 'matching'){
    matched.set <- CreateMatching(dataset, ...)
    return(matched.set)
  }

  if (is.element(ci.appr, c('weighting', 'adjusting'))){
    stop(paste(ci.appr, " casual inference approach is not implemented."))
  }

  stop('The code should get here. Something is wrong with checing arguments.')
}
