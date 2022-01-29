#' @title
#' Check Additional Arguments
#'
#' @description
#' Checks additional arguments that user needs to provide for different
#' prediction models.
#'
#' @param pred_model The prediction model.
#' @param ci_appr The causal inference approach.
#' @param use_cov_transform A logical value (TRUE/FALSE) to use covariate balance
#' transforming.
#' @param transformers A list of transformers.
#' @param ...  Additional named arguments passed.
#'
#' @return
#' TRUE if requirements are met. Raises error otherwise.
#'
#' @keywords internal
#'
#'
check_args <- function(pred_model, ci_appr,
                       use_cov_transform, transformers,
                       gps_model, trim_quantiles,
                       optimized_compile, ...){

  # 1) Check if the main arguments are correct.
  # 2) Generate required arguments based on main arguments.
  # 3) Check if based on the main argument, the required arguments are provided.
  # 4) Check if the provided required arguments' values are acceptable.

  # ------------------------------------------------------

  required_args <- max_attempt <-  NULL

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  check_args_estimate_gps(pred_model, gps_model, ...)
  check_args_generate_pseudo_pop(max_attempt = max_attempt)
  check_args_compile_pseudo_pop(ci_appr, use_cov_transform,
                                transformers, trim_quantiles,
                                optimized_compile, ...)
  check_args_use_cov_transformers(use_cov_transform, transformers)

  invisible(TRUE)
}


#' @title
#' Check estimate_gps function arguments
#'
#' @description
#' Checks estimate_gps function arguments to make sure that the required
#' additional arguments are provided.
#'
#' @param pred_model The selected prediction model.
#' @param ... Additional arguments to successfully run the selected pred_model.
#'
#' @return
#' Returns True if passes all checks, successfully. Otherwise raises ERROR.
#'
#' @keywords internal
#'
check_args_estimate_gps <- function(pred_model, gps_model, ...){

  required_args <- NULL

  # checkpoint 1 -----------------------------------------
  if (!is.element(pred_model, c('sl'))){
    stop(paste(pred_model, " is not a valid prediction model."))
  }

  if (!is.element(gps_model, c('parametric','non-parametric'))){
    stop(paste(gps_model, " is not a valide gps_model.",
               "Valid options: parametric, non-parametric."))
  }

  # checkpoint 2 ------------------------------------------
  if (pred_model == 'sl'){
    required_args <- c(required_args, 'sl_lib')
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

check_args_generate_pseudo_pop <- function(max_attempt){

  if (!is.numeric(max_attempt)){
    stop(paste(max_attempt, " is not acceptible for max_attempt. Should be a
                 numeric value."))
  }

  invisible(TRUE)
}



#' @title
#' Check compile_pseudo_pop function arguments
#'
#' @description
#  Checks compile_pseudo_pop function arguments to make sure that the required
#' additional arguments are provided.
#'
#' @param ci.appr The selected causal inference approach.
#' @param ...  Additional arguments to successfully run the selected ci_appr.
#'
#' @return
#' Returns True if passes all checks, successfully. Otherwise raises ERROR.
#'
#' @keywords internal
#'
check_args_compile_pseudo_pop <- function(ci_appr, use_cov_transform,
                                          transformers, trim_quantiles,
                                          optimized_compile, ...){

  # Passing packaging check() ----------------------------
  covar_bl_method <- NULL
  matching_fun <- NULL
  max_attempt <- NULL
  covar_bl_trs_type <- NULL

  required_args <- NULL

  # checkpoint 1 -----------------------------------------
  #if (!is.element(ci_appr, c('matching','weighting','adjusting'))){
  if (!is.element(ci_appr, c('matching', 'weighting'))){
    stop(paste(ci_appr, " is not a valid causal inference approach."))
  }

  if (!is.numeric(trim_quantiles)){
    stop("trim_quantiles should be numeric values.")
  }

  if ((trim_quantiles[1] < 0 || trim_quantiles[1] > 1) ||
      (trim_quantiles[2] < 0 || trim_quantiles[2] > 1) ||
      (trim_quantiles[1] > trim_quantiles[2])){
    stop(paste("trim_quntiles should be in the [0,1] range,",
               " and the first quantile should be less than the second one."))
  }

  if (!is.logical(optimized_compile)){
    stop(paste("optimized_compile: ", optimized_compile," is not valid.",
    "It should be a logical value."))
  }


  # checkpoint 2 ------------------------------------------
  if (ci_appr == 'matching'){
    required_args <- c(required_args, 'covar_bl_method', 'covar_bl_trs',
                       'covar_bl_trs_type','matching_fun', 'delta_n', 'scale')
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

    if (scale < 0 || scale > 1){
      stop(paste("scale shoule be in [0,1] range. Current provided value: ",
                 scale))
    }
  }

  if (!is.element(covar_bl_trs_type, c('mean', 'median', 'maximal'))){
    stop(paste(covar_bl_method, " is not a valid covar balance type.",
               " Available options: mean, median, maximal."))
  }


  invisible(TRUE)
}


#' @title
#' Check Covariate Balance Transformers Argument
#'
#' @description
#' Checks Covariate Balance Transformers in terms of using them and available
#' transformers.
#'
#' @param use_cov_transform A logical value (TRUE/FALSE) to use covariate balance
#' transforming.
#' @param transformers A list of transformers.
#'
#' @keywords internal
#'
#' @return
#' TRUE if passes all tests.
check_args_use_cov_transformers <- function(use_cov_transform,
                                          transformers){

  # Passing packaging check() ----------------------------
  # None

  # checkpoint 1 -----------------------------------------
  if (!is.logical(use_cov_transform)){
    stop(paste("use_cov_transform should be TRUE or FALSE. Current value: ",
               use_cov_transform))
  }

  if (!is.list(transformers)){
    stop(paste("transformers expects a list of transformerns. Current type: ",
               typeof(transformers)))
  }

  # checkpoint 2 -----------------------------------------
  # None
  # checkpoint 3 -----------------------------------------
  # None
  # checkpoint 4 -----------------------------------------
  # None

  invisible(TRUE)
}
