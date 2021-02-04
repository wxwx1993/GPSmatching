#' Funtction to develop prediction model based on user's preferences.
#'
#' @param Y Target data
#' @param X Input data
#' @param model Prediction model algorithm.
#'   - 'sl': SuperLearner
#' @param ... Model related parameters should be provided.
#'
#' @return
#' return value (TODO)
#'
#' @keywords internal
#'


train_it <- function(Y, X, model, sl.lib) {

  if (model == 'sl'){
    #TODO: We assume parameters are provided, and if not the package
    # itself will raise error. Double-check the params.
    #TODO: other models should be able to pass arbitrary data.
    pr_mdl <- SuperLearner(Y=Y, X=data.frame(X), SL.library=sl.lib)
    return(pr_mdl)
  } else {
    stop('The requested model has not been implemented.')
  }

}

