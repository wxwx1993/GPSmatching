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
#' @return
#' Returns A Boolean value. TRUE if the modified library for the given library is
#' implemented; otherwise, it returns FALSE. This function is called for side
#' effects.
#'
gen_wrap_sl_lib <- function(lib_name, params, nthread){

  # ------------------------------------------------------
  xgb_nrounds <- xgb_eta <- xgb_max_depth <- xgb_min_child_weight <- NULL
  rgr_num.trees <- rgr_write.forest <- rgr_replace <- rgr_verbose <- NULL
  rgr_family <- NULL
  # ------------------------------------------------------


  if (lib_name == "m_xgboost"){
    xgb_default_params = list("xgb_nrounds"=100,
                              "xgb_max_depth"=6,
                              "xgb_eta"=0.3,
                              "xgb_min_child_weight"=1
                             )
    for (item in names(params)){

      if (!is.null(xgb_default_params[[item]])){
        # the parameter belongs to xgboost family
        # choose one value at random
        if (length(params[[item]])==1) {
          new_val <- params[[item]]
        } else {
          new_val <- sample(params[[item]],1)
        }
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


    logger::log_debug("Hyperparameters for m_xgboost: ntrees: {xgb_nrounds}, ",
                      " eta: {xgb_eta}, max_depth: {xgb_max_depth}, ",
                      " min_child_weight: {xgb_min_child_weight}.")
    return(TRUE)

  } else if (lib_name == "m_ranger"){
    rgr_default_params = list("rgr_num.trees"=500,
                              "rgr_write.forest"=TRUE,
                              "rgr_replace"=TRUE,
                              "rgr_verbose"=FALSE,
                              "rgr_family"= "Gaussian"
    )

    for (item in names(params)){

      if (!is.null(rgr_default_params[[item]])){
        # the parameter belongs to ranger family
        # choose one value at random
        if (length(params[[item]])==1) {
          new_val <- params[[item]]
        } else {
          new_val <- sample(params[[item]],1)
        }
        # assign that value to the default list
        rgr_default_params[[item]] <- new_val
      }
    }

    list2env(rgr_default_params, environment())
    eval(parse(text= paste(" m_ranger_internal <- function(num.threads = ",
                           nthread,
                           ", num.trees = ", rgr_num.trees,
                           ", write.forest = ", rgr_write.forest,
                           ", replace = ", rgr_replace,
                           ", verbose = ", rgr_verbose,
                           ", family = ", rgr_family,
                           ",...) {SuperLearner::SL.ranger(num.threads = num.threads,",
                           "num.trees = num.trees, replace=replace,",
                           "verbose=verbose, family=family, ",
                           "...)}", sep="")), envir = .GlobalEnv)

    logger::log_debug("Hyperparameters for m_ranger: num.trees: {rgr_num.trees}, ",
                      " write.forest: {rgr_write.forest}, replace: {rgr_replace}, ",
                      " verbose: {rgr_verbose}, family: {rgr_family}.")

    return(TRUE)
  } else {
    message(paste("Modified library for ", lib_name, " is not implemented.",
                  "Will be used as provided."))
    return(FALSE)
  }
}
