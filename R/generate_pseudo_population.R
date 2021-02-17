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
#' @param ci_appr The causal inference approach. Possible values are:
#'   - "matching": Matching by GPS
#'   - "weighting": Weighting by GPS
#'   - "adjusting": Adjusting by GPS
#' @param running_appr The running approach.
#'   - "base": Base implementation
#'   - "parallel": Uses parallel flag whenever possible. (Currently is activated
#'    on the SuperLearner Module.)
#' @param pred_model a prediction model
#' @param save_output If TRUE, output results will be stored at the save.path.
#'  Default is FALSE.
#' @param save_path location for storing the final results, format of the saved
#' file will be detected by the file name extension.
#' @param ...  Additional arguments passed to different models.
#' @details
#' ## Additional parameters
#' ### Causal Inference Approach (ci.appr)
#' - if ci.appr = 'matching':
#'   - *matching_fun*: Matching function. Available options:
#'     - matching_l1: Manhattan distance matching
#'   - *delta_n*: caliper parameter.
#'   - *scale*: a specified scale parameter to control the relative weight that
#'  is attributed to the distance measures of the exposure versus the GPS.
#'   - *covar_bl_method*: covariate balance method. Available options:
#'      - 'absolute'
#'   - *covar_bl_trs*: covariate balance threshold
#'   - *max_attempt*: maximum number of attempt to satisfy covariate balance.
#'   - See [create_matching()] for more details about the parameters and default
#'   values.
#' - if ci.appr = 'weightig':
#'   - *covar_bl_method*: Covariate balance method.
#'   - *covar_bl_trs*: Covariate balance threshold
#'   - *max_attempt*: Maximum number of attempt to satisfy covariate balance.
#' ### Prediction models (pred_model)
#' - if pred_model = 'sl':
#'   - *sl.lib*: A vector of prediction algorithms.
#'
#' @return
#' \code{GenPseudoPop} returns a data.table pseudo population that is generated
#' or augmented based on the selected causal inference approach (ci_appr).
#'
#' @export
#' @examples
#' m_d <- gen_syn_data(sample_size = 100)
#' pseuodo_pop <- gen_pseudo_pop(m_d$Y,
#'                               m_d$treat,
#'                               m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                               ci_appr = "matching",
#'                               running_appr = "base",
#'                               pred_model = "sl",
#'                               sl_lib = c("SL.xgboost","SL.earth","SL.gam"),
#'                               covar_bl_method = "absolute",
#'                               covar_bl_trs = 0.1,
#'                               max_attempt = 1,
#'                               matching_fun = "matching_l1",
#'                               delta_n = 1,
#'                               scale = 0.5)
#'
gen_pseudo_pop <- function(Y,
                           w,
                           c,
                           ci_appr,
                           running_appr,
                           pred_model,
                           save_output = FALSE,
                           save_path = NULL,
                           ...){

  # Passing packaging check() ------------------------------
  max_attempt <- NULL
  # --------------------------------------------------------


  ## Check arguments ---------------------------------------
  check_args(pred_model,ci_appr,running_appr, ...)

  ## Generate output Set -----------------------------------
  counter <- 1

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  while (counter < max_attempt+1){

    ## Estimate GPS -----------------------------
    estimate_gps_out <- estimate_gps(Y, w, c, pred_model, running_appr,
                                     internal_use = TRUE, ...)

    ## Compile data ---------
    pseudo_pop <- compile_pseudo_pop(dataset=estimate_gps_out,
                                     ci_appr=ci_appr, ...)

    if (ci_appr == 'adjust'){
      # No covariate balance test for the 'adjust' causal inference approach.
      break
    }

    if (check_covar_balance(pseudo_pop, ci_appr, ...)){
      message(paste('Covariate balance condition has been met (iteration: ',
                    counter,'/', max_attempt,')'))
      break
    }

    counter <- counter + 1
  }

  if (counter == max_attempt+1){
    message(paste('Covariate balance condition has not been met.'))
  }

  ## Store output ---------------------------------

  if (save_output){
    if (!missing(save_path)){
      #TODO: Implement a function to write the output into disk or database.
      message('Saving data on disk is not implemented.')
    } else {
      warning('The output for storing data is not provided. This command is
              ignored.')
    }
  }
  invisible(pseudo_pop)
}
