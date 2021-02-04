#' Funtction to develop prediction model based on user's preferences.
#'
#' @param y A vector of target data. TODO: change these names to avoid confusion.
#' @param x A vector, matrix, or dataframe of input data.
#' @param model Prediction model algorithm.
#'   - 'sl': SuperLearner The required parameters:
#'     - *sl.lib*: a set of methods used for estimating target value (e.g.,
#'     ("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
#' @param ... Model related parameters should be provided.
#'
#' @return
#' prediction model
#'
#' @importFrom SuperLearner SuperLearner
#' @keywords internal
#'
TrainIt <- function(Y, X, pred.model, ...) {

  # Passing packaging check() ----------------------------
  sl.lib <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  if (pred.model == 'sl'){
    pr_mdl <- SuperLearner(Y=Y, X=data.frame(X), SL.library=sl.lib)
    return(pr_mdl)
  } else {
    stop(' This should not be raised. Something is wrong with CheckArgs
         function.')
  }
}
