#' @title
#' Generate pseudo population
#'
#' @description
#' Generates pseudo population data set based on user-defined causal inference
#' approach. The function uses an adaptive approach to satisfies covariate
#' balance requirements. The function terminates either by satisfying covariate
#' balance or completing the requested number of iteration, whichever comes
#' first.
#'
#' @param Y A data.frame comprised of two columns: one contains the observed
#' outcome variable, and the other is labeled as 'id'. The column for the
#' outcome variable can be assigned any name as per your requirements.
#' @param w A data.frame comprised of two columns: one contains the observed
#' exposure variable, and the other is labeled as 'id'. The column for the
#' outcome variable can be assigned any name as per your requirements.
#' @param c A data.frame of includes observed covariate variables. It should
#' also consist of a column named 'id'.
#' @param ci_appr The causal inference approach. Possible values are:
#'   - "matching": Matching by GPS
#'   - "weighting": Weighting by GPS
#' @param gps_density Model type which is used for estimating GPS value,
#' including `normal` (default) and `kernel`.
#' @param use_cov_transform If TRUE, the function uses transformer to meet the
#'  covariate balance.
#' @param transformers A list of transformers. Each transformer should be a
#' unary function. You can pass name of customized function in the quotes.
#' Available transformers:
#'   - pow2: to the power of 2
#'   - pow3: to the power of 3
#' @param bin_seq Sequence of w (treatment) to generate pseudo population. If
#' NULL is passed the default value will be used, which is
#' `seq(min(w)+delta_n/2,max(w), by=delta_n)`.
#' @param exposure_trim_qtls A numerical vector of two. Represents the trim quantile
#' level for exposure values. Both numbers should be in the range of \[0,1] and
#' in increasing order (default: c(0.01, 0.99)).
#' @param gps_trim_qtls A numerical vector of two. Represents the trim quantile
#' level for the gps values. Both numbers should be in the range of \[0,1] and
#' in increasing order (default: c(0.0, 1.0)).
#' @param params Includes list of params that is used internally. Unrelated
#'  parameters will be ignored.
#' @param sl_lib A vector of prediction algorithms.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
#' @param include_original_data If TRUE, includes the original data in the
#' outcome.
#' @param gps_obj A gps object that is generated with `estimate_gps` function.
#' If it is provided, the number of iteration will forced to 1 (Default: NULL).
#' @param ...  Additional arguments passed to different models.
#' @details
#' ## Additional parameters
#' ### Causal Inference Approach (ci.appr)
#' - if ci.appr = 'matching':
#'   - *dist_measure*: Matching function. Available options:
#'     - l1: Manhattan distance matching
#'   - *delta_n*: caliper parameter.
#'   - *scale*: a specified scale parameter to control the relative weight that
#'  is attributed to the distance measures of the exposure versus the GPS.
#'   - *covar_bl_method*: covariate balance method. Available options:
#'      - 'absolute'
#'   - *covar_bl_trs*: covariate balance threshold
#'   - *covar_bl_trs_type*: covariate balance type (mean, median, maximal)
#'   - *max_attempt*: maximum number of attempt to satisfy covariate balance.
#'   - See [create_matching()] for more details about the parameters and default
#'   values.
#' - if ci.appr = 'weighting':
#'   - *covar_bl_method*: Covariate balance method.
#'   - *covar_bl_trs*: Covariate balance threshold
#'   - *max_attempt*: Maximum number of attempt to satisfy covariate balance.
#'
#'
#' @return
#' Returns a pseudo population (gpsm_pspop) object that is generated
#' or augmented based on the selected causal inference approach (ci_appr). The
#' object includes the following objects:
#' - params
#'   - ci_appr
#'   - params
#' - pseudo_pop
#' - adjusted_corr_results
#' - original_corr_results
#' - best_gps_used_params
#' - effect size of generated pseudo population
#'
#' @export
#' @examples
#' m_d <- generate_syn_data(sample_size = 100)
#' pseuoo_pop <- generate_pseudo_pop(m_d[, c("id", "Y")],
#'                                   m_d[, c("id", "w")],
#'                                   m_d[, c("id", "cf1","cf2","cf3","cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   gps_density = "normal",
#'                                   bin_seq = NULL,
#'                                   expos_trim_qlts = c(0.01,0.99),
#'                                   gps_trim_qlts = c(0.01,0.99),
#'                                   use_cov_transform = FALSE,
#'                                   transformers = list(),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                                 xgb_eta=c(0.1,0.2,0.3)),
#'                                   sl_lib = c("m_xgboost"),
#'                                   nthread = 1,
#'                                   covar_bl_method = "absolute",
#'                                   covar_bl_trs = 0.1,
#'                                   covar_bl_trs_type= "mean",
#'                                   max_attempt = 1,
#'                                   dist_measure = "l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
generate_pseudo_pop <- function(Y,
                                w,
                                c,
                                ci_appr,
                                gps_density = "normal",
                                use_cov_transform = FALSE,
                                transformers = list("pow2","pow3"),
                                bin_seq = NULL,
                                exposure_trim_qtls = c(0.01, 0.99),
                                gps_trim_qtls = c(0.0, 1.0),
                                params = list(),
                                sl_lib = c("m_xgboost"),
                                nthread = 1,
                                include_original_data = FALSE,
                                gps_obj = NULL,
                                ...){

  # Passing packaging check() ------------------------------
  max_attempt <- NULL
  covar_bl_trs <- NULL
  covar_bl_trs_type <- NULL

  # Log system info
  log_system_info()

  # timing the function
  st_time_gpp <- proc.time()

  # function call
  fcall <- match.call()

  # Check arguments ----------------------------------------
  check_args(ci_appr, use_cov_transform, transformers,
             gps_density, exposure_trim_qtls, ...)

  # Generate output set ------------------------------------
  counter <- 0

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i, unlist(dot_args[i], use.names = FALSE))
  }

  covariate_cols <- Filter(function(x) x != "id", colnames(c))
  exposure_col <- Filter(function(x) x != "id", colnames(w))
  outcome_col <- Filter(function(x) x != "id", colnames(Y))

  # TODO: check for data quality.

  prep_results <- preprocess_data(Y, w, c, exposure_trim_qtls, exposure_col)
  tmp_data <- prep_results$preprocessed_data
  original_data <- prep_results$original_data

  # Retrieve data.
  Y <- tmp_data[, c("id", outcome_col)]
  w <- tmp_data[, c("id", exposure_col)]
  c <- tmp_data[, c("id", covariate_cols)]

  original_corr_obj <- check_covar_balance(
                          w = tmp_data[, c(exposure_col)],
                          c = tmp_data[, c(covariate_cols)],
                          counter_weight = NULL,
                          ci_appr = ci_appr,
                          nthread = nthread,
                          ...)
  tmp_data <- NULL

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  # transformed_vals is a list of lists. Each internal list's first element is
  # the column name and the rest is operands that is applied to it.
  # TODO: this needs a dictionary style data structure.

  transformed_vals <- lapply(covariate_cols, function(x) c(x))
  c_extended <- c
  c_original <- c
  recent_swap <- NULL
  best_ach_covar_balance <- NULL

  if (!is.null(gps_obj)){
    if (!inherits(gps_obj, "cgps_gps")){
      stop("Provided gps_obj is not an standard gps object.")
    }
    max_attempt <- 1
    logger::log_info("Maximum attemp was forced to 1 (gps_obj is provided).")
  }

  while (counter < max_attempt) {

    counter <- counter + 1

    ## Estimate GPS -----------------------------
    logger::log_debug("Started to estimate gps ... ")
    if (is.null(gps_obj)) {
      estimate_gps_out <- estimate_gps(w,
                                       c_extended[, c("id", covariate_cols)],
                                       gps_density,
                                       params = params,
                                       sl_lib = sl_lib,
                                       nthread = nthread,
                                       ...)
    } else {
      estimate_gps_out <- gps_obj
    }
    # trim gps -----------------------------------
    estimate_gps_out <- trim_gps(estimate_gps_out, gps_trim_qtls)

    gps_used_params <- estimate_gps_out$used_params
    zero_initialize <- rep(0, nrow(estimate_gps_out$dataset))
    estimate_gps_out$dataset$counter_weight <- zero_initialize
    logger::log_debug("Finished estimating gps.")

    # Dropping the transformed column ------------
    if (!is.null(recent_swap)){
      # first element is old_col name
      # second element is new_col name
      new_col_ind <- which(covariate_cols == recent_swap[2])
      covariate_cols <- covariate_cols[-new_col_ind]
      covariate_cols[length(covariate_cols)+1] <- recent_swap[1]
      c_extended[[recent_swap[2]]] <- NULL
      logger::log_debug("Transformed column {recent_swap[2]} was reset to {recent_swap[1]}.")
    }

    ## Compile data -------------------------------
    logger::log_debug("Started compiling pseudo population ... ")
    pseudo_pop <- compile_pseudo_pop(data_obj = estimate_gps_out,
                                     ci_appr = ci_appr,
                                     gps_density = gps_density,
                                     bin_seq = bin_seq,
                                     exposure_col_name = exposure_col,
                                     nthread = nthread,
                                     ...)

    pseudo_pop_y <- merge(Y, pseudo_pop, by = "id")
    if (nrow(pseudo_pop_y) == 0){
      stop(paste0("Merged data length is 0.",
                  " Make sure that Y and pseudo_pop belong to the same",
                  " observations, ",
                  " or partially include same observations."))
    }

    pseudo_pop <- merge(pseudo_pop_y, c, by = "id")
    if (nrow(pseudo_pop) == 0){
      stop(paste0("Merged data length is 0.",
                  " Make sure that c and pseudo_pop belong to the same",
                  " observations, ",
                  " or partially include same observations."))
    }

    logger::log_debug("Finished compiling pseudo population.")

    # check covariate balance
    adjusted_corr_obj <- check_covar_balance(
                           w = pseudo_pop[, c(exposure_col)],
                           c = pseudo_pop[, covariate_cols],
                           counter_weight = pseudo_pop[,
                                         c("counter_weight")],
                           ci_appr = ci_appr,
                           nthread = nthread,
                           ...)

    # check Kolmogorov-Smirnov statistics
    ks_stats <- check_kolmogorov_smirnov(w = pseudo_pop[, c(exposure_col)],
                                         c = pseudo_pop[, covariate_cols],
                                         counter_weight = pseudo_pop[,
                                                           c("counter_weight")],
                                         ci_appr = ci_appr,
                                         nthread = nthread)

    covar_bl_t <- paste0(covar_bl_trs_type, "_absolute_corr")

    if (is.null(best_ach_covar_balance)){
      best_ach_covar_balance <- getElement(adjusted_corr_obj$corr_results,
                                           covar_bl_t)
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
      best_gps_used_params <- gps_used_params
      best_ks_stats <- ks_stats
    }

    if (getElement(adjusted_corr_obj$corr_results,covar_bl_t) <
        best_ach_covar_balance) {
      best_ach_covar_balance <- getElement(adjusted_corr_obj$corr_results,
                                           covar_bl_t)
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
      best_gps_used_params <- gps_used_params
      best_ks_stats <- ks_stats
    }

    if (adjusted_corr_obj$pass) {
      message(paste('Covariate balance condition has been met (iteration: ',
                    counter, '/', max_attempt, ')', sep = ""))
      break
    }

    if (use_cov_transform) {

      logger::log_debug("------------ Started conducting covariate transform ...")

      sort_by_covar <- sort(adjusted_corr_obj$corr_results$absolute_corr,
                            decreasing = TRUE)

      value_found = FALSE
      for (c_name in names(sort_by_covar)) {
        # find the element index in the transformed_vals list
        el_ind <- which(unlist(lapply(transformed_vals,
                                      function(x){ x[1] == c_name })))

        if (length(el_ind) == 0) {
          # wants to choose a transformed column, which indicates that the
          # the transformation was not helpful. Move to the next worst covariate balance.
          next
        }

        if (is.factor(c_extended[[c_name]])) {
          # Only numerical values are considered for transformation.
          next
        }

        logger::log_debug("Feature with the worst covariate balance: {c_name}.",
                          " Located at index {el_ind}.")

        for (operand in transformers) {
          if(length(transformed_vals[[el_ind]]) > 1) {
            if (!is.element(operand,
                            transformed_vals[[el_ind]][
                              2:length(transformed_vals[[el_ind]])])){
                new_c <- c_name
                new_op <- operand
                value_found = TRUE
                break
            }
          } else {
            new_c <- c_name
            new_op <- operand
            value_found = TRUE
            break
          }
        }
        if (value_found){break}
      }

      if (!value_found){
        logger::log_info(paste(
                    "All possible combination of transformers has been tried.",
                    "Retrying ... .", sep=" "))

        # removed used transformers on covariate balance.
        transformed_vals <- lapply(covariate_cols, function(x) c(x))
        recent_swap <- NULL
        if (sum(sort(colnames(c)) != sort(colnames(c_extended))) > 0) {
          logger::log_error("At this step, c and c_extended should be the same, doublecheck.")
          c_extended <- c
        }
        next
      } else {

      # add operand into the transformed_vals
      transformed_vals[[el_ind]][length(transformed_vals[[el_ind]])+1] <- new_op


      t_dataframe <- transform_it(new_c, c_extended[[new_c]], new_op)

      c_extended <- cbind(c_extended, t_dataframe)
      recent_swap <- c(new_c, unlist(colnames(t_dataframe)))
      index_to_remove <- which(unlist(covariate_cols) == new_c)
      covariate_cols <- covariate_cols[-index_to_remove]
      covariate_cols[length(covariate_cols) + 1] <- unlist(colnames(t_dataframe))
      logger::log_debug("In the next iteration (if any) feature {c_name}",
                        " will be replaced by {unlist(colnames(t_dataframe))}.")
      }
      logger::log_debug("------------ Finished conducting covariate transform.")
    }
  }

  if (!adjusted_corr_obj$pass){
    message(paste('Covariate balance condition has not been met.'))
  }

  message(paste0("Best ",covar_bl_trs_type," absolute correlation: ",
                 best_ach_covar_balance,
                 "| Covariate balance threshold: ", covar_bl_trs))


  # compute effective sample size
  ess_recommended <- length(Y) / 10
  ess <- ((sum(best_pseudo_pop$counter_weight) ^ 2) /
          sum(best_pseudo_pop$counter_weight ^ 2))
  if (ess < ess_recommended){
      logger::log_warn("Effective sample size is less than recommended.",
                       "Current: {ess}, recommended min value:",
                       " {ess_recommended}.")
  }

  result <- list()
  class(result) <- "gpsm_pspop"

  result$params$ci_appr <- ci_appr
  result$params$params <- params
  for (item in arg_names){
    result$params[[item]] <- get(item)
  }


  if (include_original_data){
    result$original_data <- original_data
  }


  result$pseudo_pop <- best_pseudo_pop
  result$adjusted_corr_results <- best_adjusted_corr_obj$corr_results
  result$original_corr_results <- original_corr_obj$corr_results
  result$ks_stats <- best_ks_stats
  result$fcall <- fcall
  result$passed_covar_test <- adjusted_corr_obj$pass
  result$counter <- counter
  result$ci_appr <- ci_appr
  result$best_gps_used_params <- best_gps_used_params
  result$covariate_cols_name <- unlist(covariate_cols)
  result$ess <- ess
  result$ess_recommended <- ess_recommended

  end_time_gpp <- proc.time()

  logger::log_debug("Wall clock time to run generate_pseudo_pop:",
                    " {(end_time_gpp -   st_time_gpp)[[3]]} seconds.")
  logger::log_debug("Covariate balance condition has been met (TRUE/FALSE):",
                    " {adjusted_corr_obj$pass}, (iteration:",
                    " {counter} / {max_attempt})")
  invisible(result)
}

# transformers
pow2 <- function(x) {x ^ 2}
pow3 <- function(x) {x ^ 3}

#' @title
#' Transform data
#'
#' @description
#' Transforms data into new values.
#'
#' @param c_name column (attribute) name.
#' @param c_val column value
#' @param transformer transformer function.
#'
#' @keywords internal
#'
#' @return
#'  Returns transformed data.frame.
transform_it <- function(c_name, c_val, transformer) {

  t_c_name <- paste(c_name, "_", transformer, sep = "")
  t_data <- do.call(transformer, list(c_val))
  t_data <- data.frame(t_data)
  colnames(t_data) <- t_c_name

  return(data.frame(t_data))
}


#' @title
#' Preprocess data
#'
#' @description
#' Preprocess data to isolate extra details
#'
#' @inheritParams generate_pseudo_pop
#' @param exposure_col Column name that is used for exposure.
#' @return
#' A list with preprocessed and original data.
#'
#' @keywords internal
preprocess_data <- function(Y, w, c, trim_quantiles, exposure_col){

  id_exist_Y <- any(colnames(Y) %in% "id")
  if (!id_exist_Y) stop("Y should include id column.")

  id_exist_w <- any(colnames(w) %in% "id")
  if (!id_exist_w) stop("w should include id column.")

  id_exist_c <- any(colnames(c) %in% "id")
  if (!id_exist_c) stop("c should include id column.")

  merged_12 <- merge(Y, w, by = "id")
  merged_data <- merge(merged_12, c, by = "id")

  df1 <- merged_data
  original_data <- df1

  # get trim quantiles and trim data
  q1 <- stats::quantile(df1[[exposure_col]], trim_quantiles[1])
  q2 <- stats::quantile(df1[[exposure_col]], trim_quantiles[2])

  logger::log_debug("{trim_quantiles[1]*100}% quantile for trim: {q1}")
  logger::log_debug("{trim_quantiles[2]*100}% for trim: {q2}")

  df1 <- df1[stats::complete.cases(df1), ]
  df1 <- df1[df1[[exposure_col]] <= q2  & df1[[exposure_col]] >= q1, ]

  result = list()
  result$preprocessed_data <- df1
  result$original_data <- original_data

  return(result)
}


