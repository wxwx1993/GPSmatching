#' @title
#' Smooth exposure response function
#'
#' @description
#' Smooths exposure response function based on bandwidth
#'
#' @param matched_Y A vector of the outcome variable in the matched set.
#' @param bw The bandwidth value.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param matched_cw A vector of counter or weight variable in the matched set.
#' @param kernel_appr Internal kernel approach. Available options are `locpol`
#' and `kernsmooth`.
#' @return
#' Smoothed value of ERF
#' @keywords internal
#'
smooth_erf <- function(matched_Y,
                       bw,
                       matched_w,
                       matched_cw,
                       x_eval,
                       kernel_appr) {

  if (length(bw) != 1) {
    stop("bw should be of length 1.")
  }

  if (sum(matched_cw == 0) == length(matched_cw)) {
    matched_cw <- matched_cw + 1
    logger::log_debug("Giving equal weight for all samples.")
  }

  if (kernel_appr == "locpol"){

    smoothed_val <- smooth_erf_locpol(matched_Y = matched_Y,
                                      matched_w = matched_w,
                                      matched_cw = matched_cw,
                                      x_eval = x_eval,
                                      bw = bw)
    return(smoothed_val)

  } else if (kernel_appr == "kernsmooth") {

    smoothed_val <- smooth_erf_kernsmooth(matched_Y = matched_Y,
                                          matched_w = matched_w,
                                          matched_cw = matched_cw,
                                          x_eval = x_eval,
                                          bw = bw)
    return(smoothed_val)

  } else {
    stop(paste("Acceptiable kernel_appr are: locpol and kernsmooth.",
                "The provided value: ", kernel_appr))
  }
}


#' @title
#' Compute smoothed erf with locpol approach
#'
#' @param matched_Y A vector of outcome value.
#' @param matched_w A vector of treatment value.
#' @param matched_cw  A vector of weight or count.
#' @param bw A scaler number indicating the bandwidth.
#'
#' @return
#' A vector of smoothed ERF.
#'
#' @keywords internal
smooth_erf_locpol <- function(matched_Y,
                              matched_w,
                              matched_cw,
                              x_eval,
                              bw){

  if (is.null(x_eval)){
    x_eval <- matched_w
  }

  data <- data.frame(matched_Y = matched_Y, matched_w = matched_w)
  val <- locpol::locpol(formula = matched_Y ~ matched_w,
                        data = data,
                        bw = bw,
                        weig = matched_cw,
                        xeval = x_eval,
                        kernel = locpol::gaussK)

  smoothed_val <- val$lpFit$matched_Y
  return(smoothed_val)
}



#' @title
#' Compute smoothed erf with kernsmooth approach
#'
#' @param matched_Y A vector of outcome value.
#' @param matched_w A vector of treatment value.
#' @param matched_cw  A vector of weight or count.
#' @param bw A scaler number indicating the bandwidth.
#'
#' @return
#' A vector of smoothed ERF.
#'
#' @keywords internal
smooth_erf_kernsmooth <- function(matched_Y,
                                  matched_w,
                                  matched_cw,
                                  x_eval,
                                  bw){
  # cannot handle weight
  # converting weight to count then replicating data based on count.

  # check if we are dealing with weights (not count)----------------
  is_weight <- any(matched_cw != floor(matched_cw))

  if (is_weight){
    # round to 2 decimal places.
    matched_cw_r2 <- round(matched_cw, 3)
    count_val <- matched_cw_r2 * 1000
    count_val <- as.integer(count_val)
  } else {
    count_val <- as.integer(matched_cw)
  }

  # Replicate data based on count value. ----------------------------
  data <- data.frame(matched_Y = matched_Y,
                     matched_w = matched_w,
                     count_val = count_val)
  data_rep <- data[rep(1:nrow(data),
                       data$count), , drop = FALSE]
  rownames(data_rep) <- NULL

  # drop count column
  data_rep[["count_val"]] <- NULL

  if (is.null(x_eval)){
    x_eval <- data_rep[["matched_w"]]
  }

  smoothed_val <- stats::approx(KernSmooth::locpoly(data_rep[["matched_w"]],
                                                    data_rep[["matched_Y"]],
                                                    bandwidth = bw,
                                                    gridsize=1000),
                                xout=x_eval,
                                rule=2)$y
  return(smoothed_val)
}


#' @title
#' Generate kernel function
#'
#' @description
#' Generates a kernel function
#'
#' @param t A standardized vector (z-score)
#'
#' @return
#' probability distribution
#'
#' @keywords internal
#'
#'
generate_kernel <- function(t) {
  stats::dnorm(t)
}

#' @title
#' Helper function
#'
#' @param bw bandwidth value
#' @param matched_w a vector of continuous exposure variable in matched set.
#' @param w_vals a vector of values that you want to calculate the values of
#'  the ERF at.
#'
#' @return
#' return value (TODO)
#' @keywords internal
#'
w_fun <- function(bw, matched_w, w_vals){
  w_avals <- NULL
  for (w_val in w_vals) {
    w_std <- (matched_w - w_val) / bw
    kern_std <- generate_kernel(w_std) / bw
    tmp_mean <- mean(w_std ^ 2 * kern_std)
    w_avals <- c(w_avals, tmp_mean * (generate_kernel(0) / bw) /
                   (mean(kern_std) * tmp_mean - mean(w_std * kern_std) ^ 2))
  }
  return(w_avals / length(matched_w))
}

#' @title
#' Estimate hat (fitted) values
#'
#' @description
#' Estimates the fitted values based on bandwidth value
#'
#' @param bw The bandwidth value.
#' @param matched_w A vector of continuous exposure variable in the matched set.
#' @param w_vals A vector of values that you want to calculate the values of the
#'  ERF at.
#'
#' @return
#' Returns fitted values, or the prediction made by the model for each observation.
#' @keywords internal
#'
estimate_hat_vals <- function(bw, matched_w, w_vals) {
  stats::approx(w_vals,
                w_fun(bw, matched_w, w_vals),
                xout = matched_w,
                rule=2)$y
}


