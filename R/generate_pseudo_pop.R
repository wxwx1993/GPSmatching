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
#' @param c A data.frame or matrix of observed covariates variable.
#' @param ci_appr The causal inference approach. Possible values are:
#'   - "matching": Matching by GPS
#'   - "weighting": Weighting by GPS
#'   - "adjusting": Adjusting by GPS
#' @param pred_model a prediction model (use "sl" for SuperLearner)
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
#'   - pred_model
#'   - params
#' - pseudo_pop
#' - adjusted_corr_results
#' - original_corr_results
#' - optimized_compile (True or False)
#'
#' @export
#' @examples
#' m_d <- generate_syn_data(sample_size = 100)
#' pseuoo_pop <- generate_pseudo_pop(m_d$Y,
#'                                   m_d$treat,
#'                                   m_d[c("cf1","cf2","cf3","cf4","cf5","cf6")],
#'                                   ci_appr = "matching",
#'                                   pred_model = "sl",
#'                                   gps_model = "parametric",
#'                                   bin_seq = NULL,
#'                                   trim_quantiles = c(0.01,0.99),
#'                                   optimized_compile = FALSE,
#'                                   use_cov_transform = FALSE,
#'                                   transformers = list(),
#'                                   sl_lib = c("m_xgboost"),
#'                                   params = list(xgb_nrounds=c(10,20,30),
#'                                                 xgb_eta=c(0.1,0.2,0.3)),
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
                                pred_model,
                                gps_model = "parametric",
                                use_cov_transform = FALSE,
                                transformers = list("pow2","pow3"),
                                bin_seq = NULL,
                                trim_quantiles = c(0.01,0.99),
                                optimized_compile = FALSE,
                                params = list(),
                                nthread = 1,
                                ...){

  # Passing packaging check() ------------------------------
  max_attempt <- NULL
  covar_bl_trs <- NULL
  # --------------------------------------------------------

  # Log system info
  log_system_info()

  # timing the function
  st_time_gpp <- proc.time()

  # function call
  fcall <- match.call()

  # Check arguments ----------------------------------------
  check_args(pred_model,ci_appr, use_cov_transform, transformers,
             gps_model, trim_quantiles, optimized_compile, ...)

  # Generate output set ------------------------------------
  counter <- 0

  ## collect additional arguments
  dot_args <- list(...)
  arg_names <- names(dot_args)

  for (i in arg_names){
    assign(i,unlist(dot_args[i],use.names = FALSE))
  }

  # Compute original data absolute correlation
  # The third column is reserved for gps, however, in covariate balance test we
  # do not use gps values.
  # The forth column is reserved for counter.
  # The fifth column is reserved for row_index
  # TODO: find a better place to the following code.

  q1 <- stats::quantile(w,trim_quantiles[1])
  q2 <- stats::quantile(w,trim_quantiles[2])

  logger::log_debug("{trim_quantiles[1]*100}% qauntile for trim: {q1}")
  logger::log_debug("{trim_quantiles[2]*100}% for trim: {q2}")

  tmp_data <- convert_data_into_standard_format(Y, w, c, q1, q2, ci_appr)

  original_corr_obj <- check_covar_balance(tmp_data, ci_appr, nthread,
                                           optimized_compile, ...)
  tmp_data <- NULL

  # loop until the generated pseudo population is acceptable or reach maximum
  # allowed iteration.

  if (ci_appr == "matching") internal_use=TRUE else internal_use=FALSE
  #internal_use = TRUE

  covariate_cols <- as.list(colnames(c))

  # transformed_vals is a list of lists. Each internal list's first element is
  # the column name and the rest is operands that is applied to it.
  # TODO: this need a dictionary style data structure.

  transformed_vals <- covariate_cols
  c_extended <- c
  recent_swap <- NULL
  best_ach_covar_balance <- NULL

  while (counter < max_attempt){

    counter <- counter + 1

    ## Estimate GPS -----------------------------
    logger::log_debug("Started to estimate gps ... ")
    estimate_gps_out <- estimate_gps(Y, w, c_extended[unlist(covariate_cols)],
                                     pred_model, gps_model,
                                     params = params, nthread = nthread,
                                     internal_use = internal_use, ...)
    logger::log_debug("Finished estimating gps.")

    # Dropping the transformed column ------------
    if (!is.null(recent_swap)){
      # first element is old_col name
      # second element is new_col name
      new_col_ind <- which(covariate_cols==recent_swap[2])
      covariate_cols[[new_col_ind]] <- NULL
      covariate_cols[length(covariate_cols)+1] <- recent_swap[1]
      c_extended[[recent_swap[2]]] <- NULL
      estimate_gps_out[[1]][recent_swap[2]] <- NULL
      estimate_gps_out[[1]][length(estimate_gps_out[[1]])+1] <- c[recent_swap[1]]
      logger::log_debug("Tranformed column {recent_swap[2]} was reset to {recent_swap[1]}.")
    }

    ## Compile data ---------
    logger::log_debug("Started compiling pseudo population ... ")
    pseudo_pop <- compile_pseudo_pop(dataset=estimate_gps_out, ci_appr=ci_appr,
                                     gps_model,bin_seq, nthread = nthread,
                                     trim_quantiles = trim_quantiles,
                                     optimized_compile = optimized_compile,...)
    # trim pseudo population
    pseudo_pop <- subset(pseudo_pop[stats::complete.cases(pseudo_pop) ,],
                         w <= q2  & w >= q1)
    logger::log_debug("Finished compiling pseudo population.")

    if (ci_appr == 'adjust'){
      # No covariate balance test for the 'adjust' causal inference approach.
      break
    }
    # check covariate balance
    adjusted_corr_obj <- check_covar_balance(pseudo_pop, ci_appr, nthread,
                                             optimized_compile, ...)

    if (is.null(best_ach_covar_balance)){
      best_ach_covar_balance <- adjusted_corr_obj$corr_results$mean_absolute_corr
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
    }

    if (adjusted_corr_obj$corr_results$mean_absolute_corr < best_ach_covar_balance){
      best_ach_covar_balance <- adjusted_corr_obj$corr_results$mean_absolute_corr
      best_pseudo_pop <- pseudo_pop
      best_adjusted_corr_obj <- adjusted_corr_obj
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

  message(paste("Best Mean absolute correlation: ", best_ach_covar_balance,
                "| Covariate balance threshold: ", covar_bl_trs))

  result <- list()
  class(result) <- "gpsm_pspop"

  result$params$ci_appr <- ci_appr
  result$params$pred_model <- pred_model
  result$params$params <- params
  for (item in arg_names){
    result$params[[item]] <- get(item)
  }

  result$pseudo_pop <- best_pseudo_pop
  result$adjusted_corr_results <- best_adjusted_corr_obj$corr_results
  result$original_corr_results <- original_corr_obj$corr_results
  result$fcall <- fcall
  result$passed_covar_test <- adjusted_corr_obj$pass
  result$counter <- counter
  result$ci_appr <- ci_appr
  result$optimized_compile <- optimized_compile

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
