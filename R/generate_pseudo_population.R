#' @title
#' Generate pseudo population dataset
#'
#' @description
#' Generates pseudo population dataset based on user defined causal inference
#' approach. The output dataset satisfies covariate balance requirements if
#' required for the selected causal inference approach.
#'
#' @param Y A vector of observed outcome variable.
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame or matrix of observed covariates variable.
#' @param ci.appr The causal inference approach. Possible values are:
#'   - "matching": Matching by GPS
#'   - "weighting": Weighting by GPS
#'   - "adjusting": Adjusting by GPS
#' @param pred.model a prediction model
#' @param save.output If TRUE, output results will be stored at the save.path.
#'  Default is FALSE.
#' @param save.path location for storing the final results, format of the saved
#' file will be detected by the file name extension.
#' @param ...  Additional arguments passed to different models.
#' @details
#' ## Additional parameters
#' ### Causal Inference Approach (ci.appr)
#' - if ci.appr = 'matching':
#'   - *matching.fun*: Matching function. Available options:
#'     - matching_l1: Manhattan distance matching
#'   - *delta.n*: caliper parameter.
#'   - *scale*: a specified scale parameter to control the relative weight that
#'  is attributed to the distance measures of the exposure versus the GPS.
#'   - *covar.bl.method*: covariate balance method. Available options:
#'      - 'absolute'
#'   - *covar.bl.trs*: covariate balance threshold
#'   - *max.attemp*: maximum number of attempt to satisfy covariate balance.
#'   - See [CreateMatching()] for more details about the parameters and default
#'   values.
#' - if ci.appr = 'weightig':
#'   - *covar.bl.method*: Covariate balance method.
#'   - *covar.bl.trs*: Covariate balance threshold
#'   - *max.attemp*: Maximum number of attempt to satisfy covariate balance.
#' ### Prediction models (pred.model)
#' - if pred.model = 'sl':
#'   - *sl.lib*: A vector of prediction algorithms.
#'
#' @return
#' \code{GenPseudoPop} returns a data.table pseudo population that is generated
#' or augmented based on the selected causal inference approach (ci.appr).
#'
#' @export
#' @examples
#'
#'
#' # Generate pseudo population with matching causal inference approach, using
#' # SuperLearner package, and abolute covariance balance checking.
#'
#' m.d <- GenSynData(sample.size = 100)
#' pseuodo.pop <- GenPseudoPop(m.d$Y,
#'                             m.d$treat,
#'                             m.d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                             ci.appr = "matching",
#'                             pred.model = "sl",
#'                             sl.lib = c("SL.xgboost","SL.earth","SL.gam",
#'                                        "SL.ranger"),
#'                             covar.bl.method = "absolute",
#'                             covar.bl.trs = 0.1,
#'                             max.attemp = 1,
#'                             matching.fun = "MatchingL1",
#'                             delta.n = 1,
#'                             scale = 0.5)
#'
#'
GenPseudoPop <- function(Y,
                         w,
                         c,
                         ci.appr,
                         pred.model,
                         save.output = FALSE,
                         save.path = NULL,
                         ...){


  # Passing packaging check() ----------------------------
  max.attemp <- NULL
  # ------------------------------------------------------


  ## Check arguments ---------------------------------------
  CheckArgs(pred.model,ci.appr, ...)

  ## Generate output Set -----------------------------------
  counter <- 1

  ## collect additional arguments
  dot.args <- list(...)
  arg.names <- names(dot.args)

  for (i in arg.names){
    assign(i,unlist(dot.args[i],use.names = FALSE))
  }

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  while (counter < max.attemp+1){

    ## Estimate GPS -----------------------------
    estimate.gps.out <- EstimateGPS(Y, w, c, pred.model,internal.use = TRUE,
                                    ...)

    ## Compile data ---------
    pseudo.pop <- CompilePseudoPop(dataset=estimate.gps.out,
                                 ci.appr=ci.appr, ...)

    if (ci.appr == 'adjust'){
      # No covariate balance test for the 'adjust' causal inference approach.
      break
    }

    if (CheckCovarBalance(pseudo.pop, ci.appr, ...)){
      message(paste('Covariate balance condition has been met (iteration: ',
                    counter,'/', max.attemp,')'))
      break
    }

    counter <- counter + 1
  }

  if (counter == max.attemp+1){
    message(paste('Covariate balance condition has not been met.'))
  }

  ## Store output ---------------------------------

  if (save.output){
    if (!missing(save.path)){
      #TODO: Implement a function to write the output into disk or database.
      message('Saving data on disk is not implemented.')
    } else {
      warning('The output for storing data is not provided. This command is
              ignored.')
    }
  }
  invisible(pseudo.pop)
}
