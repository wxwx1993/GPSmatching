#' Funtction to develop prediction model based on user's preferences.
#'
#' @param target A vector of target data.
#' @param input A vector, matrix, or dataframe of input data.
#' @param model Prediction model algorithm.
#'   - 'sl': SuperLearner The required parameters:
#'     - *sl.lib*: a set of methods used for estimating target value (e.g.,
#'     ("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
#' @param ... Model related parameters should be provided.
#'
#' @return
#' prediction model
#'
#' @keywords internal
#'
TrainIt <- function(target, input, pred.model, ...) {

  # Passing packaging check() ----------------------------
  sl.lib <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  if (pred.model == 'sl'){
    pr_mdl <- SuperLearner::SuperLearner(Y=target, X=data.frame(input),
                                         SL.library=sl.lib)
    return(pr_mdl)
  } else {
    stop(' This should not be raised. Something is wrong with CheckArgs
         function.')
  }
}
