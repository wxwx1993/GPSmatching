#' @title
#' Check Additional Arguments
#'
#' @description
#' Checks additional arguments that user needs to provide for different
#' prediction models.
#'
#' @param pred.model The prediction model.
#' @param ci.appr The causal inference approach.
#' @param ...  Additional named arguments passed.
#'
#' @return
#' TRUE if requirements are met. Raises error otherwise.
#' @export
#'
#' @keywords internal
#'
CheckArgs <- function(pred.model, ci.appr, ...){

  # 1) Check if the main arguments are correct.
  # 2) Generate required arguments based on main arguments.
  # 3) Check if based on the main argument, the required arguments are provided.
  # 4) Check if the provided required arguments' values are acceptable.

  # Passing packaging check() ----------------------------
  covar.bl.method <- NULL
  matching.fun <- NULL
  # ------------------------------------------------------

  required_args <- NULL

  # checkpoint 1 -----------------------------------------
  if (!is.element(pred.model, c('sl'))){
    stop(paste(pred.model, " is not a valid prediction model."))
  }

  if (!is.element(ci.appr, c('matching','weighting','adjusting'))){
    stop(paste(ci.appr, " is not a valid causal inference approach."))
  }

  # checkpoint 2 ------------------------------------------
  if (pred.model == 'sl'){
    required_args <- c(required_args, 'sl.lib')
  }

  if (ci.appr == 'matching'){
    required_args <- c(required_args, 'covar.bl.method', 'covar.bl.trs',
                       'max.attemp', 'matching.fun', 'delta.n', 'scale')
  }

  # checkpoint 3 ------------------------------------------
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (arg in required_args){
    if (!is.element(arg,arg_names)){
      stop(paste('At least one argument is not provided. Missing argument: ',
                 arg, '.'))
    }
  }

  # checkpoint 4 ------------------------------------------
  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  if (is.element(ci.appr, c('matching','weighting'))){
    if (!is.element(covar.bl.method, c('absolute'))){
      stop(paste(covar.bl.method, " is not a valid covariance balance testing
                 method."))
    }
  }

  if (is.element(ci.appr, c('matching'))){
    if (!is.element(matching.fun, c('MatchingL1'))){
      stop(paste(matching.fun, " is not a valid matching function."))
    }
  }

  invisible(TRUE)
}
