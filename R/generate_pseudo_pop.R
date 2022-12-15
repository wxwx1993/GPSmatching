#' @title
#' Generate Pseudo Population
#'
#' @description
#' Generates pseudo population data set based on user-defined causal inference
#' approach. The function uses an adaptive approach to satisfies covariate
#' balance requirements. The function terminates either by satisfying covariate
#' balance or completing the requested number of iteration, whichever comes
#' first.
#'
#' @param Y A vector of observed outcome variable.
#' @param w A vector of observed continuous exposure variable.
#' @param c A data.frame of observed covariates variable.
#' @param ci_appr The causal inference approach. Possible values are:
#'   - "matching": Matching by GPS
#'   - "weighting": Weighting by GPS
#' @param gps_model Model type which is used for estimating GPS value, including
#' parametric (default) and non-parametric.
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
#' @param trim_quantiles A numerical vector of two. Represents the trim quantile
#' level. Both numbers should be in the range of \[0,1] and in increasing order
#' (default: c(0.01,0.99)).
#' @param optimized_compile If TRUE, uses counts to keep track of number of
#' replicated pseudo population.
#' @param params Includes list of params that is used internally. Unrelated
#'  parameters will be ignored.
#' @param sl_lib A vector of prediction algorithms.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
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
#'   - *covar_bl_trs_type*: covariate balance type (mean, median, maximal)
#'   - *max_attempt*: maximum number of attempt to satisfy covariate balance.
#'   - See [create_matching()] for more details about the parameters and default
#'   values.
#' - if ci.appr = 'weighting':
#'   - *covar_bl_method*: Covariate balance method.
#'   - *covar_bl_trs*: Covariate balance threshold
#'   - *max_attempt*: Maximum number of attempt to satisfy covariate balance.
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
#' - optimized_compile (True or False)
#' - best_gps_used_params
#' - effect size of generated pseudo population
#'
#' @export
#' @examples
#' m_d <- generate_syn_data(sample_size = 100)
#' pseuoo_pop <- generate_pseudo_pop(m_d$Y,
#'                                   m_d$treat,
#'                                   m_d[c("cf1","cf2","cf3",
#'                                         "cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   gps_model = "parametric",
#'                                   bin_seq = NULL,
#'                                   trim_quantiles = c(0.01,0.99),
#'                                   optimized_compile = FALSE,
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
#'                                   matching_fun = "matching_l1",
#'                                   delta_n = 1,
#'                                   scale = 0.5)
#'
generate_pseudo_pop <- function(Y,
                                w,
                                c,
                                ci_appr,
                                gps_model = "parametric",
                                use_cov_transform = FALSE,
                                transformers = list("pow2","pow3"),
                                bin_seq = NULL,
                                trim_quantiles = c(0.01,0.99),
                                optimized_compile = FALSE,
                                params = list(),
                                sl_lib = c("m_xgboost"),
                                nthread = 1,
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
             gps_model, trim_quantiles, optimized_compile, ...)

  # Generate output set ------------------------------------
  counter <- 0

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  covariate_cols <- as.list(colnames(c))


  # Depreciation messages
  if (!optimized_compile){
    warning("optimized_compile = FALSE will be depreciated.",
            call. = FALSE)
  }

  # get trim quantiles and trim data
  q1 <- stats::quantile(w,trim_quantiles[1])
  q2 <- stats::quantile(w,trim_quantiles[2])

  logger::log_debug("{trim_quantiles[1]*100}% quantile for trim: {q1}")
  logger::log_debug("{trim_quantiles[2]*100}% for trim: {q2}")

  # Drop data with missing values
  # Trim data based on quantiles.
  tmp_data <- cbind(Y, w, c)
  tmp_data <- tmp_data[stats::complete.cases(tmp_data),]
  tmp_data <- tmp_data[tmp_data$w <= q2  & tmp_data$w >= q1, ]

  # Retrieve data.
  Y <- tmp_data$Y
  w <- tmp_data$w
  c <- tmp_data[, unlist(covariate_cols)]

  # generating temporal data to compute covariate
  # balance based on trimmed data.
  # tmp_data <- cbind(w, c)
  # tmp_data <- subset(tmp_data[stats::complete.cases(tmp_data) ,],
  #                    w <= q2  & w >= q1)
  tmp_data <- data.table(tmp_data)
  original_corr_obj <- check_covar_balance(w = tmp_data[, c("w")],
                                           c = tmp_data[, unlist(covariate_cols),
                                                          with = FALSE],
                                           counter_weight = NULL,
                                           ci_appr = ci_appr,
                                           nthread = nthread,
                                           optimized_compile = optimized_compile,
                                           ...)
  tmp_data <- NULL

  if (ci_appr == "matching") internal_use=TRUE else internal_use=FALSE

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  # transformed_vals is a list of lists. Each internal list's first element is
  # the column name and the rest is operands that is applied to it.
  # TODO: this needs a dictionary style data structure.

  transformed_vals <- covariate_cols
  c_extended <- c
  recent_swap <- NULL
  best_ach_covar_balance <- NULL

  while (counter < max_attempt){

    counter <- counter + 1

    ## Estimate GPS -----------------------------
    logger::log_debug("Started to estimate gps ... ")
    estimate_gps_out <- estimate_gps(Y, w, c_extended[unlist(covariate_cols)],
                                     gps_model,
                                     params = params,
                                     sl_lib = sl_lib,
                                     nthread = nthread,
                                     internal_use = internal_use, ...)
    gps_used_params <- estimate_gps_out$used_params
    logger::log_debug("Finished estimating gps.")

    # Dropping the transformed column ------------
    if (!is.null(recent_swap)){
      # first element is old_col name
      # second element is new_col name
      new_col_ind <- which(covariate_cols==recent_swap[2])
      covariate_cols[[new_col_ind]] <- NULL
      covariate_cols[length(covariate_cols)+1] <- recent_swap[1]
      c_extended[[recent_swap[2]]] <- NULL
      estimate_gps_out$dataset[recent_swap[2]] <- NULL
      estimate_gps_out$dataset[length(estimate_gps_out$dataset)+1] <- c[recent_swap[1]]
      logger::log_debug("Tranformed column {recent_swap[2]} was reset to {recent_swap[1]}.")
    }

    ## Compile data -------------------------------
    logger::log_debug("Started compiling pseudo population ... ")
    pseudo_pop <- compile_pseudo_pop(data_obj = estimate_gps_out,
                                     ci_appr = ci_appr,
                                     gps_model = gps_model,
                                     bin_seq = bin_seq,
                                     nthread = nthread,
                                     optimized_compile = optimized_compile,...)
    # trim pseudo population
    logger::log_debug("Finished compiling pseudo population.")

    # check covariate balance
    adjusted_corr_obj <- check_covar_balance(w = pseudo_pop[, c("w")],
                                             c = pseudo_pop[,
                                                            unlist(covariate_cols),
                                                            with = FALSE],
                                             counter_weight = pseudo_pop[,
                                                           c("counter_weight")],
                                             ci_appr = ci_appr,
                                             nthread = nthread,
                                             optimized_compile = optimized_compile,
                                             ...)
    # check Kolmogorov-Smirnov statistics
    ks_stats <- check_kolmogorov_smirnov(w = pseudo_pop[, c("w")],
                                         c = pseudo_pop[,
                                                        unlist(covariate_cols),
                                                        with = FALSE],
                                         counter_weight = pseudo_pop[,
                                                           c("counter_weight")],
                                         ci_appr = ci_appr,
                                         nthread = nthread,
                                         optimized_compile = optimized_compile)

    covar_bl_t <- paste0(covar_bl_trs_type,"_absolute_corr")
    if (is.null(best_ach_covar_balance)){
      best_ach_covar_balance <- getElement(adjusted_corr_obj$corr_results,covar_bl_t)
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
      best_gps_used_params <- gps_used_params
      best_ks_stats <- ks_stats
    }

    if (getElement(adjusted_corr_obj$corr_results,covar_bl_t) < best_ach_covar_balance){
      best_ach_covar_balance <- getElement(adjusted_corr_obj$corr_results,covar_bl_t)
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
      best_gps_used_params <- gps_used_params
      best_ks_stats <- ks_stats
    }

    if (adjusted_corr_obj$pass){
      message(paste('Covariate balance condition has been met (iteration: ',
                    counter,'/', max_attempt,')', sep = ""))
      break
    }

    if (use_cov_transform){

      logger::log_debug("------------ Started conducting covariate transform ...")

      sort_by_covar <- sort(adjusted_corr_obj$corr_results$absolute_corr,
                            decreasing = TRUE)

      value_found = FALSE
      for (c_name in names(sort_by_covar)){
        # find the element index in the transformed_vals list
        el_ind <- which(unlist(lapply(transformed_vals,
                                      function(x){ x[1] == c_name })))

        if (length(el_ind)==0){
          # wants to choose a transformed column, which indicates that the
          # the transformation was not helpful. Move to the next worst covariate balance.
          next
        }

        if (is.factor(c_extended[[c_name]])){
          # Only numerical values are considered for transformation.
          next
        }

        logger::log_debug("Feature with the worst covariate balance: {c_name}.",
                          " Located at index {el_ind}.")

        for (operand in transformers){
          if(length(transformed_vals[[el_ind]])>1){
            if (!is.element(operand,
                            transformed_vals[[el_ind]][2:length(transformed_vals[[el_ind]])])){
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
        warning(paste("All possible combination of transformers has been tried.",
                      "Retrying ... .", sep=" "))

        # removed used transformers on covariate balance.
        transformed_vals <- covariate_cols
        recent_swap <- NULL
        if (sum(sort(colnames(c)) != sort(colnames(c_extended)))>0){
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
      index_to_remove <- which(unlist(covariate_cols)==new_c)
      covariate_cols[[index_to_remove]] <- NULL
      covariate_cols[length(covariate_cols)+1] <- unlist(colnames(t_dataframe))
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
  if (optimized_compile){
  ess_recommended <- length(Y)/10
  ess <- ((sum(best_pseudo_pop$counter_weight)^2)/
          sum(best_pseudo_pop$counter_weight^2))
    if (ess < ess_recommended){
      logger::log_warn("Effective sample size is less than recommended.",
                       "Current: {ess}, recommended min value:",
                       " {ess_recommended}.")
    }
  } else {
    ess <- NULL
    ess_recommended <- NULL
  }


  result <- list()
  class(result) <- "gpsm_pspop"



  result$params$ci_appr <- ci_appr
  result$params$params <- params
  for (item in arg_names){
    result$params[[item]] <- get(item)
  }

  result$pseudo_pop <- best_pseudo_pop
  result$adjusted_corr_results <- best_adjusted_corr_obj$corr_results
  result$original_corr_results <- original_corr_obj$corr_results
  result$ks_stats <- best_ks_stats
  result$fcall <- fcall
  result$passed_covar_test <- adjusted_corr_obj$pass
  result$counter <- counter
  result$ci_appr <- ci_appr
  result$optimized_compile <- optimized_compile
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
pow2 <- function(x) {x^2}
pow3 <- function(x) {x^3}

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
transform_it <- function(c_name, c_val, transformer){

  t_c_name <- paste(c_name,"_",transformer, sep = "")
  t_data <- do.call(transformer, list(c_val))
  t_data <- data.frame(t_data)
  colnames(t_data) <- t_c_name

  return(data.frame(t_data))
}
