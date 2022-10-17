#' @title
#' Estimate GPS Values
#'
#' @description
#' Estimates GPS value for each observation using parametric or non-parametric
#' approaches.
#'
#'
#' @param Y A vector of observed outcome variable.
#' @param w A vector of observed continuous exposure variable.
#' @param c A data frame of observed covariates variable.
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric (default) and non-parametric.
#' @param internal_use If TRUE will return helper vectors as well. Otherwise,
#'  will return original data + GPS values.
#' @param params Includes list of parameters that are used internally. Unrelated
#'  parameters will be ignored.
#' @param sl_lib A vector of prediction algorithms.
#' @param nthread An integer value that represents the number threads to be used
#' in a shared memory system.
#' @param ...  Additional arguments passed to the model.
#'
#' @return
#' The function returns a S3 object. Including the following:
#'   - Original data set + GPS, counter, row_index values (Y, w, GPS,
#'   counter_weight, row_index, c)
#'   - e_gps_pred
#'   - e_gps_std_pred
#'   - w_resid
#'   - gps_mx (min and max of gps)
#'   - w_mx (min and max of w).
#'   - used_params
#'
#' @note
#' If \code{internal.use} is set to be FALSE, only original data set + GPS will
#' be returned.
#'
#' The outcome variable is not used in estimating the GPS value. However, it is
#' used in compiling the data set with GPS values.
#'
#'
#' @export
#'
#' @examples
#' m_d <- generate_syn_data(sample_size = 100)
#' data_with_gps <- estimate_gps(m_d$Y,
#'                               m_d$treat,
#'                               m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                               gps_model = "parametric",
#'                               internal_use = FALSE,
#'                               params = list(xgb_max_depth = c(3,4,5),
#'                                        xgb_nrounds=c(10,20,30,40,50,60)),
#'                               nthread = 1,
#'                               sl_lib = c("m_xgboost")
#'                              )
#'
estimate_gps <- function(Y,
                         w,
                         c,
                         gps_model = "parametric",
                         internal_use = TRUE,
                         params = list(),
                         sl_lib = c("m_xgboost"),
                         nthread = 1,
                         ...){

  start_time <- proc.time()

  # Check passed arguments -----------------------------------------------------
  check_args_estimate_gps(gps_model, ...)

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # Check if data has missing value(s) -----------------------------------------
  if (sum(is.na(w)) > 0){
    logger::log_warn("Vector w has {sum(is.na(w))} missing values.")
  }

  if (sum(is.na(c)) > 0){
    logger::log_warn(
      "Confounders data.frame (c) has {sum(is.na(c))} missing values.")
  }

  # Generate SL wrapper library for each type of prediction algorithms ---------
  sl_lib_internal = NULL
  used_params <- list()
  for (item in sl_lib){
    wrapper_generated_res <- gen_wrap_sl_lib(lib_name = item, params,
                                             nthread = nthread)
    if (wrapper_generated_res[[1]]){
      sl_lib_internal <- c(sl_lib_internal,paste(item,"_internal", sep=""))
      used_params <- c(used_params, wrapper_generated_res[[2]])
    } else {
      sl_lib_internal <- c(sl_lib_internal, item)
    }
  }

  if (gps_model == "parametric"){

    e_gps <- train_it(target = w, input = c,
                      sl_lib_internal = sl_lib_internal, ...)
    e_gps_pred <- e_gps$SL.predict
    e_gps_std_pred <- stats::sd(w - e_gps_pred)
    w_resid <- compute_resid(w,e_gps_pred,e_gps_std_pred)
    gps <- stats::dnorm(w, mean = e_gps_pred, sd = e_gps_std_pred)

  } else if (gps_model == "non-parametric"){

    e_gps <- train_it(target = w, input = c,
                      sl_lib_internal = sl_lib_internal, ...)
    e_gps_pred <- e_gps$SL.predict
    e_gps_std <- train_it(target = abs(w-e_gps_pred), input = c,
                           sl_lib_internal = sl_lib_internal, ...)
    e_gps_std_pred <- e_gps_std$SL.predict
    w_resid <- compute_resid(w,e_gps_pred,e_gps_std_pred)
    gps <- compute_density(w_resid, w_resid)

  } else {

    logger::log_error("Code should nevet get here. Doublecheck check_arguments.")
    stop(paste("Invalide gps_model: ", gps_model,
               ". Use parametric or non-parametric."))
  }

  w_mx <- compute_min_max(w)
  gps_mx <- compute_min_max(gps)
  counter_weight <- (w*0)+0 # initialize counter.
  row_index <- seq(1,length(w),1) # initialize row index.
  dataset <- cbind(Y,w,gps,counter_weight,row_index, c)

  # Logging for debugging purposes
  logger::log_debug("Min Max of treatment: {paste(w_mx, collapse = ', ')}")
  logger::log_debug("Min Max of gps: {paste(gps_mx, collapse = ', ')}")
  logger::log_debug("Weights for the select libraries in predicting e_gps:",
          " {paste(names(e_gps$coef), collapse = ', ')}",
          " {paste(e_gps$coef, collapse = ', ')}",
          " | Overal Risk: {sum(e_gps$coef * e_gps$cvRisk)/length(e_gps$coef)}")
  logger::log_debug("Wall clock time to estimate e_gps:",
                    " {e_gps$times$everything[3]} seconds.")
  if (gps_model == "non-parametric"){
    logger::log_debug("Weights for the select libraries in predicting residuals:",
            " {paste(names(e_gps_std$coef), collapse = ', ')}",
            " {paste(e_gps_std$coef, collapse = ', ')} | Overal risk:",
            " {sum(e_gps_std$coef * e_gps_std$cvRisk)/length(e_gps_std$coef)}")
    logger::log_debug("Wall clock time to estimate residuals:",
                      " {e_gps_std$times$everything[3]} seconds.")
  }

  end_time <- proc.time()

  logger::log_debug("Wall clock time to run estimate_gps function: ",
                    " {(end_time - start_time)[[3]]} seconds.")


  result <- list()
  class(result) <- "cgps_gps"
  result$dataset <- dataset
  result$used_params <- used_params

  if (internal_use){
    result$e_gps_pred <- e_gps_pred
    result$e_gps_std_pred <- e_gps_std_pred
    result$w_resid <- w_resid
    result$gps_mx <- gps_mx
    result$w_mx <- w_mx
  }

  invisible(result)
}
