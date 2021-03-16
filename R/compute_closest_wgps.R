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
#'
#' @return
#' The function returns index of subset data that is closest to the original data
#' sample.
#'
#' @keywords internal
#'
compute_closest_wgps <- function(a, b, c, d, sc, nthread=1){

  if (!is.numeric(a) ||
      !is.numeric(b) ||
      !is.numeric(c) ||
      !is.numeric(d) ||
      !is.numeric(sc)){
    stop('Input values for compute_closest_wgps should be numeric.')
  }

  if (length(d) != 1){
    stop('Expecting a scaler number for d.')
  }


  if (length(sc) != 1){
    stop('Expecting a scaler number for sc(scale).')
  }

  if (length(a) != length(c)){
    stop('Expecting equal length for a and c.')
  }


  # TODO: choose chunk size based on the subset data size.
  chunk_size = 1000

  fun1 <- function(index, a, b, cd, sc, chunk_size){

    if ((index+chunk_size)>length(b)){
      n_index <- length(b)
    } else {
      n_index <- index + chunk_size -1
    }
    tmp_matrix <- apply(compute_outer(a, b[index:n_index], '-') * sc,
                        2,
                        function(x) which.min(cd + x))

  }


  platform_os <- .Platform$OS.type

  if (is.element(platform_os,c("unix"))){

    logger::log_debug("mclapply is used in computing wgps. nthread: {nthread}")


    index_list <- seq(1, length(b), chunk_size)
    c_minus_d <- abs(c-d)*(1-sc)
    p_wm <- parallel::mclapply(index_list,
                               fun1,
                               a=a,
                               b=b,
                               cd=c_minus_d,
                               sc=sc,
                               chunk_size=chunk_size,
                               mc.cores=nthread)

    wm <- unlist(p_wm)
  } else {
    wm <- apply(compute_outer(a, b, '-') * sc,
                2,
                function(x) which.min(abs(c - d) * (1 - sc) + x))
  }

  return(wm)
}
