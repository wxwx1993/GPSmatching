#' @title
#' Check Additional Arguments
#'
#' @description
#' Checks additional arguments that user needs to provide for different
#' prediction models.
#'
#' @param pred_model The prediction model.
#' @param ci_appr The causal inference approach.
#' @param running_appr The running approach.
#' @param ...  Additional named arguments passed.
#'
#' @return
#' TRUE if requirements are met. Raises error otherwise.
#' @export
#'
#' @keywords internal
#'
check_args <- function(pred_model, ci_appr, running_appr, ...){

  # 1) Check if the main arguments are correct.
  # 2) Generate required arguments based on main arguments.
  # 3) Check if based on the main argument, the required arguments are provided.
  # 4) Check if the provided required arguments' values are acceptable.


  # ------------------------------------------------------

  required_args <- NULL

  check_args_estimate_gps(pred_model, running_appr, ...)
  check_args_compile_pseudo_pop(ci_appr, ...)

  invisible(TRUE)
}

#' @title
#' Check EstimateGPS function arguments
#'
#' @description
#' Checks EstimateGPS function arguments to make sure that the required
#' additional arguments are provided.
#'
#' @param pred_model The selected prediction model.
#' @param ... Addional arguments to successfully run the selected pred.model.
#'
#' @return
#' Returns True if passes all checks, successfully. Otherwise raises ERROR.
#'
#' @keywords internal
#'
check_args_estimate_gps <- function(pred_model, running_appr, ...){

  required_args <- NULL

  # checkpoint 1 -----------------------------------------
  if (!is.element(pred_model, c('sl'))){
    stop(paste(pred_model, " is not a valid prediction model."))
  }

  if (!is.element(running_appr,c('base', 'parallel'))){
    stop(paste(running_appr, " is not a valid running approach."))
  }

  # checkpoint 2 ------------------------------------------
  if (pred_model == 'sl'){
    required_args <- c(required_args, 'sl.lib')
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
  # None for this test.

  invisible(TRUE)
}




#' @title
#' Check CompilePseudoPop function arguments
#'
#' @description
#  Checks CompilePseudoPop function arguments to make sure that the required
#' additional arguments are provided.
#'
#' @param ci.appr The selected causal inference approach.
#' @param ...  Additional arguments to successfully run the selected ci.appr.
#'
#' @return
#' Returns True if passes all checks, successfully. Otherwise raises ERROR.
#'
#' @keywords internal
#'
check_args_compile_pseudo_pop <- function(ci_appr, ...){

  # Passing packaging check() ----------------------------
  covar_bl_method <- NULL
  matching_fun <- NULL

  required_args <- NULL

  # checkpoint 1 -----------------------------------------
  if (!is.element(ci_appr, c('matching','weighting','adjusting'))){
    stop(paste(ci_appr, " is not a valid causal inference approach."))
  }

  # checkpoint 2 ------------------------------------------
  if (ci_appr == 'matching'){
    required_args <- c(required_args, 'covar_bl_method', 'covar_bl_trs',
                       'max_attempt', 'matching_fun', 'delta_n', 'scale')
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

  if (is.element(ci_appr, c('matching','weighting'))){
    if (!is.element(covar_bl_method, c('absolute'))){
      stop(paste(covar_bl_method, " is not a valid covariance balance testing
                 method."))
    }
  }

  if (is.element(ci_appr, c('matching'))){
    if (!is.element(matching_fun, c('matching_l1'))){
      stop(paste(matching_fun, " is not a valid matching function."))
    }
  }
  invisible(TRUE)
}
