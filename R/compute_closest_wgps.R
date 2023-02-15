#' @title
#' Find the closest data in subset to the original data
#'
#' @description
#' A function to compute the closest data in subset of data to the original data
#' based on two attributes: vector and scalar (vector of size one).
#'
#' @param a  Vector of the first attribute values for subset of data.
#' @param b  Vector of the first attribute values for all data.
#' @param c  Vector of the second attribute values for subset of data.
#' @param d  Vector of size one for the second attribute value.
#' @param sc Scale parameter to give weight for two mentioned measurements.
#' @param nthread Number of available cores.
#'
#' @return
#' The function returns index of subset data that is closest to the original data
#' sample.
#'
#' @keywords internal
#'
compute_closest_wgps <- function(a, b, c, d, sc, nthread) {

  if (!is.numeric(a) ||
      !is.numeric(b) ||
      !is.numeric(c) ||
      !is.numeric(d) ||
      !is.numeric(sc)) {
    stop('Input values for compute_closest_wgps should be numeric.')
  }

  if (length(a) < 1 ||
      length(b) < 1 ||
      length(c) < 1 ) {
    stop('Input values for compute_closest_wgps cannot be empty values.')
  }

  if (length(d) != 1) {
    stop('Expecting a scaler number for d.')
  }

  if (length(sc) != 1) {
    stop('Expecting a scaler number for sc(scale).')
  }

  if (length(a) != length(c)) {
    stop('Expecting equal length for a and c.')
  }

  if (sc < 0 || sc > 1 ) {
    stop('Expecting sc in [0,1] range.')
  }

  logger::log_trace("Size of subset of data: {length(a)}")

  if (sc != 1 ) {
   c_minus_d <- abs(c - d) * (1 - sc)
   wm <- compute_closest_wgps_helper(a, b, c_minus_d, sc, nthread)
  } else {
   logger::log_trace("Simplified approach was selected in matching.")

   start_time_before <- proc.time()

   # sort b
   original_data_index <- seq(1, length(a), 1)
   sorted_a <- sort(a, decreasing = FALSE)

   # keep the index
   initial_a_order <- order(a, decreasing = FALSE)
   end_time_before <- proc.time()
   logger::log_trace("Wall clock time to sort and order:",
                     " {(end_time_before - start_time_before)[[3]]} seconds.")
   # compute_closest
   # return back index
   sorted_value_index <- compute_closest_wgps_no_sc_binary_search(sorted_a,
                                                                  b,
                                                                  nthread)
   start_time_after <- proc.time()
   wm <- initial_a_order[sorted_value_index]
   end_time_after <- proc.time()
   logger::log_trace("Wall clock time to reproduce original index:",
                     " {(end_time_after - start_time_after)[[3]]} seconds.")
  }
   return(wm)
}
