#' @title
#' Generate Prediction Model
#'
#' @description
#' Function to develop prediction model based on user's preferences.
#'
#' @param target A vector of target data.
#' @param input A vector, matrix, or dataframe of input data.
#' @param sl_lib_internal The internal library to be used by SuperLearner
#' @param ... Model related parameters should be provided.
#'
#' @return
#' prediction model
#'
#' @keywords internal
#'
train_it <- function(target,
                     input,
                     sl_lib_internal = NULL,
                     ...) {

  # Passing packaging check() ----------------------------
  sl_lib <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  platform_os <- .Platform$OS.type

  pr_mdl <- SuperLearner::SuperLearner(Y = target,
                                       X = data.frame(input),
                                       SL.library = sl_lib_internal)
  return(pr_mdl)
}
