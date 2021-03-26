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
#' @param c A data.frame or matrix of observed covariates variable.
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
#' @param params Includes list of params that is used internally. Unrelated
#'  parameters will be ignored.
#' @param nthread An integer value that represents then number threads to use by
#'  internal packages.
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
#'   - *sl_lib*: A vector of prediction algorithms.
#'
#' @return
#' Returns a pseudo population (gpsm_pspop) object that is generated
#' or augmented based on the selected causal inference approach (ci_appr). The
#' object includes the following objects:
#' - params
#'   - ci_appr
#'   - running_appr
#'   - pred_model
#'   - params
#' - pseudo_pop
#' - adjusted_corr_results
#' - original_corr_results
#'
#' @export
#' @examples
#' m_d <- gen_syn_data(sample_size = 100)
#' pseuoo_pop <- gen_pseudo_pop(m_d$Y,
#'                              m_d$treat,
#'                              m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                              ci_appr = "matching",
#'                              running_appr = "base",
#'                              pred_model = "sl",
#'                              sl_lib = c("m_xgboost"),
#'                              params = list(xgb_nrounds=c(10,20,30),
#'                               xgb_eta=c(0.1,0.2,0.3)),
#'                              nthread = 1,
#'                              covar_bl_method = "absolute",
#'                              covar_bl_trs = 0.1,
#'                              max_attempt = 1,
#'                              matching_fun = "matching_l1",
#'                              delta_n = 1,
#'                              scale = 0.5)
#'
gen_pseudo_pop <- function(Y,
                           w,
                           c,
                           ci_appr,
                           running_appr,
                           pred_model,
                           save_output = FALSE,
                           save_path = NULL,
                           params = list(),
                           nthread = 1,
                           ...){

  # Passing packaging check() ------------------------------
  max_attempt <- NULL
  # --------------------------------------------------------

  # function call
  fcall <- match.call()

  # Check arguments ----------------------------------------
  check_args(pred_model,ci_appr,running_appr, ...)

  # Generate output set ------------------------------------
  counter <- 1

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # Compute original data absolute correlation
  # The third column is reserved for gps, however, in covariate balance test we
  # do not use gps values.
  # TODO: find a better place to the following code.
  tmp_data <- cbind(Y,w,w,c)
  original_corr_obj <- check_covar_balance(tmp_data, ci_appr, nthread, ...)
  tmp_data <- NULL

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  if (ci_appr == "matching") internal_use=TRUE else internal_use=FALSE

  while (counter < max_attempt+1){

    ## Estimate GPS -----------------------------
    logger::log_debug("Started to estimate gps ... ")
    estimate_gps_out <- estimate_gps(Y, w, c, pred_model, running_appr,
                                     params = params, nthread = nthread,
                                     internal_use = internal_use, ...)
    logger::log_debug("Finished estimating gps.")

    ## Compile data ---------
    logger::log_debug("Started compiling pseudo population ... ")
    pseudo_pop <- compile_pseudo_pop(dataset=estimate_gps_out,
                                     ci_appr=ci_appr, nthread = nthread, ...)
    logger::log_debug("Finished compiling pseudo population")

    if (ci_appr == 'adjust'){
      # No covariate balance test for the 'adjust' causal inference approach.
      break
    }

    logger::log_debug("Started checking covariate balance ... ")
    adjusted_corr_obj <- check_covar_balance(pseudo_pop, ci_appr, nthread, ...)
    logger::log_debug("Finished checking covariate balance ... ")


    if (adjusted_corr_obj$pass){
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
  result <- list()
  class(result) <- "gpsm_pspop"

  result$params$ci_appr <- ci_appr
  result$params$running_appr <- running_appr
  result$params$pred_model <- pred_model
  result$params$params <- params
  for (item in arg_names){
    result$params[[item]] <- get(item)
  }

  result$pseudo_pop <- pseudo_pop
  result$adjusted_corr_results <- adjusted_corr_obj$corr_results
  result$original_corr_results <- original_corr_obj$corr_results
  result$fcall <- fcall

  invisible(result)
}
