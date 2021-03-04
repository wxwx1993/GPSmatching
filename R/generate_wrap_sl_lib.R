#' @title
#' Generate customized wrapper for SuperLearner libraries
#'
#' @description
#' The function generates customized wrapper in order to have access to the
#' external libraries hyperparameters.
#'
#' @param lib_name The library name (e.g., `m_xgboost`).
#' @param params A list that includes key-values for different parameters. Only
#'   relevant parameters will be extracted, others will be ignored.
#' @param nthread Number of threads available to be used by external libraries
#'  (in case they can use it).
#'
#' @keywords internal
#'
gen_wrap_sl_lib <- function(lib_name, params, nthread){

  # ------------------------------------------------------
  xgb_nrounds <- xgb_eta <- xgb_max_depth <- xgb_min_child_weight <- NULL
  # ------------------------------------------------------


  if (lib_name == "m_xgboost"){
    xgb_default_params = list("xgb_nrounds"=100,
                              "xgb_max_depth"=4,
                              "xgb_eta"=0.1,
                              "xgb_min_child_weight"=10
                              #"xgb_gamma"=0,
                              #"xgb_max_delta_step"=0,
                              #"xgb_subsample"=1,
                              #"xgb_sampling_method"="uniform",
                              #"xgb_lambda" = 1,
                              #"xgb_alpha" = 0,
                              #"xgb_num_parallel_tree" = 1
                             )
    for (item in names(params)){

      if (xgb_default_params[[item]]){
        # the parameter belongs to xgboost family
        # choose one value at random
        new_val = sample(params[[item]],1)
        # assign that value to the default list
        xgb_default_params[[item]] <- new_val
      }
    }

    list2env(xgb_default_params, environment())
    eval(parse(text= paste(" m_xgboost_internal <- function(nthread = ",
                           nthread,
                           ", ntrees = ", xgb_nrounds,
                           ", shrinkage = ",xgb_eta,
                           ", max_depth = ",xgb_max_depth,
                           ", minobspernode = ",xgb_min_child_weight,
                           ",...) {SuperLearner::SL.xgboost(nthread = nthread,",
                           "ntrees = ntrees, shrinkage=shrinkage,",
                           "max_depth=max_depth, minobspernode = minobspernode,",
                           "...)}", sep="")), envir = .GlobalEnv)

    # eval(parse(text= paste(" m_xgboost_internal <- function(ntrees = ",
    #                        xgb_ntrees,
    #                        ", max_depth = ", xgb_max_depth,
    #                        ", eta = ", xgb_eta,
    #                        ", min_child_weight =", xgb_min_child_weight,
    #                        ", gamma = ", xgb_gamma,
    #                        ", max_delta_step = ",  xgb_max_delta_step,
    #                        ", subsample = ", xgb_subsample,
    #                        ", sampling_method = \"", xgb_sampling_method,"\"",
    #                        ", lambda = ", xgb_lambda,
    #                        ", alpha = ", xgb_alpha,
    #                        ", num_parallel_tree =", xgb_num_parallel_tree,
    #                        ", nthread = ", nthread, ") {
    #   SL.xgboost(ntrees = ntrees,
    #              max_depth = max_depth,
    #              eta = eta,
    #              min_child_weight = min_child_weight,
    #              gamma = gamma,
    #              max_delta_step = max_delta_step,
    #              subsample = subsample,
    #              sampling_method = sampling_method,
    #              lambda = lambda,
    #              alpha = alpha,
    #              num_parallel_tree = num_parallel_tree,
    #              nthread = nthread,
    #              verbose = 0,
    #              ...)
    # }", sep="")), envir = .GlobalEnv)

  } else {
    message(paste("Modified library for ", lib_name, " is not implemented."))
  }
}

