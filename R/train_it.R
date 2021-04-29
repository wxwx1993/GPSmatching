#' Funtction to develop prediction model based on user's preferences.
#'
#' @param target A vector of target data.
#' @param input A vector, matrix, or dataframe of input data.
#' @param pred_model Prediction model algorithm.
#'   - 'sl': SuperLearner The required parameters:
#'     - *sl_lib*: a set of methods used for estimating target value (e.g.,
#'     ("SL.xgboost","SL.earth","SL.gam","SL.ranger"))
#' @param running_appr The running approach.
#' @param sl_lib_internal The internal library to be used by SuperLearner
#' @param ... Model related parameters should be provided.
#'
#' @return
#' prediction model
#'
#' @keywords internal
#'
train_it <- function(target, input, pred_model, running_appr,
                     sl_lib_internal=NULL, ...) {

  # Passing packaging check() ----------------------------
  sl_lib <- NULL
  # ------------------------------------------------------

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  platform_os <- .Platform$OS.type

  if (pred_model == 'sl'){

    if (running_appr=="parallel"){
      if (is.element(platform_os,c("unix"))){
        pr_mdl <- SuperLearner::mcSuperLearner(Y=target, X=data.frame(input),
                                               SL.library=sl_lib_internal)
      } else {
        message(paste("Running on multiple cores is not implemented for ",
                       platform_os, " platform. Running on single core ..."))
        pr_mdl <- SuperLearner::SuperLearner(Y=target, X=data.frame(input),
                                             SL.library=sl_lib_internal)
        }
    } else if (running_appr=="base") {

      pr_mdl <- SuperLearner::SuperLearner(Y=target, X=data.frame(input),
                                           SL.library=sl_lib_internal)
    } else {
      stop(' The requested running approach (',running_appr,
           ') is not implemented.')
    }
      return(pr_mdl)

    } else {
    stop(' This should not be raised. Something is wrong with CheckArgs
         function.')
  }
}