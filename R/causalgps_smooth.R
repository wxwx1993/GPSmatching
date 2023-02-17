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
w_fun <- function(bw, matched_w, matched_cw, w_vals){
    w_avals <- NULL
    for (w_val in w_vals) {
    w_std <- (matched_w - w_val) / bw
    kern_std <- generate_kernel(w_std) / bw
    tmp_mean <- weighted.mean(w_std ^ 2 * kern_std, w = matched_cw)
    w_avals <- c(w_avals, tmp_mean * (generate_kernel(0) / bw) /
                   (weighted.mean(kern_std, w = matched_cw) * tmp_mean - weighted.mean(w_std * kern_std, w = matched_cw) ^ 2))
  }
  return(w_avals / sum(matched_cw))
}
