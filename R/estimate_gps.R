#' @title
#' Estimate generalized propensity score (GPS) values
#'
#' @description
#' Estimates GPS value for each observation using normal or kernel
#' approaches.
#'
#'
#' @param w A data frame of observed continuous exposure variable. Including
#' `id` and `w` columns.
#' @param c A data frame of observed covariates variable. Also includes `id`
#' column.
#' @param gps_density Model type which is used for estimating GPS value,
#' including `normal` (default) and `kernel`.
#' @param params Includes list of parameters that are used internally. Unrelated
#'  parameters will be ignored.
#' @param sl_lib A vector of prediction algorithms.
#' @param nthread An integer value that represents the number threads to be used
#' in a shared memory system.
#' @param ...  Additional arguments passed to the model.
#'
#' @return
#' The function returns a S3 object. Including the following:
#'   - `dataset `: `id`, `w`, `gps`
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
#' data_with_gps <- estimate_gps(m_d[, c("id", "w")],
#'                               m_d[, c("id", "cf1", "cf2", "cf3",
#'                                       "cf4", "cf5", "cf6")],
#'                               gps_density = "normal",
#'                               params = list(xgb_max_depth = c(3,4,5),
#'                                        xgb_nrounds=c(10,20,30,40,50,60)),
#'                               nthread = 1,
#'                               sl_lib = c("m_xgboost")
#'                              )
#'
estimate_gps <- function(w,
                         c,
                         gps_density = "normal",
                         params = list(),
                         sl_lib = c("m_xgboost"),
                         nthread = 1,
                         ...) {

  start_time <- proc.time()

  # Check passed arguments -----------------------------------------------------
  check_args_estimate_gps(gps_density, ...)


  id_exist_w <- any(colnames(w) %in% "id")
  if (!id_exist_w) stop("w should include id column.")

  id_exist_c <- any(colnames(c) %in% "id")
  if (!id_exist_c) stop("c should include id column.")

  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
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
      sl_lib_internal <- c(sl_lib_internal, paste(item, "_internal", sep=""))
      used_params <- c(used_params, wrapper_generated_res[[2]])
    } else {
      sl_lib_internal <- c(sl_lib_internal, item)
    }
  }

  merged_data <- merge(w, c, by = "id")

  if (nrow(merged_data) == 0){
    stop(paste0("Merged data length is 0.",
                " Make sure that w and c belong to the same observations, ",
                " or partially include same observations."))
  }

  exposure_col <- Filter(function(x) !(x %in% c("id")), colnames(w))
  covariate_cols <- Filter(function(x) !(x %in% c("id")), colnames(c))


  if (gps_density == "normal"){
    e_gps <- train_it(target = merged_data[,c(exposure_col)],
                      input = merged_data[, covariate_cols],
                      sl_lib_internal = sl_lib_internal,
                      ...)

    e_gps_pred <- e_gps$SL.predict
    e_gps_std_pred <- stats::sd(merged_data[,c(exposure_col)] - e_gps_pred)
    w_resid <- compute_resid(merged_data[,c(exposure_col)],
                             e_gps_pred,
                             e_gps_std_pred)
    gps <- stats::dnorm(merged_data[,c(exposure_col)],
                        mean = e_gps_pred,
                        sd = e_gps_std_pred)

  } else if (gps_density == "kernel"){

    e_gps <- train_it(target = merged_data[,c(exposure_col)],
                      input = merged_data[, covariate_cols],
                      sl_lib_internal = sl_lib_internal, ...)
    e_gps_pred <- e_gps$SL.predict
    e_gps_std <- train_it(target = abs(merged_data[,c(exposure_col)] - e_gps_pred),
                          input = merged_data[, covariate_cols],
                          sl_lib_internal = sl_lib_internal, ...)
    e_gps_std_pred <- e_gps_std$SL.predict
    w_resid <- compute_resid(merged_data[,c(exposure_col)],
                             e_gps_pred,e_gps_std_pred)
    gps <- compute_density(w_resid, w_resid)

  } else {

    logger::log_error("Code should nevet get here. Doublecheck check_arguments.")
    stop(paste("Invalide gps_density: ", gps_density,
               ". Use normal or kernel."))
  }

  w_mx <- compute_min_max(merged_data[,c(exposure_col)])
  gps_mx <- compute_min_max(gps)
  merged_data$gps <- gps

  # Drop covariates
  merged_data[covariate_cols] <- NULL
  dataset <- merged_data
  dataset$e_gps_pred <- e_gps_pred
  if (length(e_gps_std_pred) == 1){
    e_gps_std_pred <- rep(e_gps_std_pred, nrow(dataset))
  }
  dataset$e_gps_std_pred <- e_gps_std_pred
  dataset$w_resid <- w_resid

  # Logging for debugging purposes
  logger::log_debug("Min Max of treatment: {paste(w_mx, collapse = ', ')}")
  logger::log_debug("Min Max of gps: {paste(gps_mx, collapse = ', ')}")
  logger::log_debug("Weights for the select libraries in predicting e_gps:",
          " {paste(names(e_gps$coef), collapse = ', ')}",
          " {paste(e_gps$coef, collapse = ', ')}",
          " | Overal Risk: {sum(e_gps$coef * e_gps$cvRisk)/length(e_gps$coef)}")
  logger::log_debug("Wall clock time to estimate e_gps:",
                    " {e_gps$times$everything[3]} seconds.")
  if (gps_density == "kernel"){
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
  result$gps_mx <- gps_mx
  result$w_mx <- w_mx

  invisible(result)
}
