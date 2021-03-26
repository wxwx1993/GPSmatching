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

  if (length(a) < 1 ||
      length(b) < 1 ||
      length(c) < 1 ){
    stop('Input values for compute_closest_wgps cannot be empty values.')
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

  if (sc < 0 || sc > 1 ){
    stop('Expecting sc in [0,1] range.')
  }

  # Maximum allocated memory for this task: 4 GB.
  # 1024^3 = 1073741824
  # double value: 8 bytes


  # Estimating index with Rcpp and mclapply
   chunk_size = ceiling(length(b)/nthread)
   index_list <- seq(1, length(b), chunk_size)


   myfun <- function(index, a, b, c_minus_d, sc, chunk_size){

     if ((index+chunk_size)>length(b)){
       n_index <- length(b)
     } else {
       n_index <- index + chunk_size -1
     }

     wm_cpp <- compute_closest_wgps_helper(a, b[index:n_index], c_minus_d, sc)
     return(wm_cpp)
    }

   c_minus_d <- abs(c-d)*(1-sc)

   wm_cpp_mc <- mclapply(index_list,
                         myfun,
                         a = a,
                         b = b,
                         c_minus_d = c_minus_d,
                         sc = sc,
                         chunk_size = chunk_size,
                         mc.cores = nthread)

   wm_cpp_mc <- unlist(wm_cpp_mc)

   # Estimating index with Rcpp and without mclapply
   wm_cpp <- compute_closest_wgps_helper(a, b, c_minus_d, sc)


   # Original implementation
   max_allocated_mem = 0.1
   chunk_size = floor((max_allocated_mem*1073741824)/(length(a)*8))
   logger::log_debug(paste("Length of all data: {length(b)},",
                          " length of subset of data: {length(a)},",
                          " max_allocated_mem: {max_allocated_mem},",
                          " chunk size: {chunk_size} "))

  fun1 <- function(index, a, b, cd, sc, chunk_size){

    if ((index+chunk_size)>length(b)){
      n_index <- length(b)
    } else {
      n_index <- index + chunk_size -1
    }

    outer_val <- compute_outer(a, b[index:n_index], '-') * sc
    result_vec <- apply(outer_val,
                        2,
                        function(x) which.min(cd + x))
    outer_val <- NULL
    return(result_vec)
  }

  platform_os <- .Platform$OS.type

  if (is.element(platform_os,c("unix"))){

    logger::log_debug("mclapply is used in computing wgps. nthread: {nthread}")

    index_list <- seq(1, length(b), chunk_size)
    c_minus_d <- abs(c-d)*(1-sc)
    N <- length(index_list)
    i_list <- splitIndices(N, ceiling(N/(nthread)))
    result_list <- list()

    for (i in seq_along(i_list)){
      i_vec <- i_list[[i]]
      result_list[i_vec] <- mclapply(index_list[i_vec],
                                     fun1,
                                     a=a,
                                     b=b,
                                     cd=c_minus_d,
                                     sc=sc,
                                     chunk_size=chunk_size,
                                     mc.silent = FALSE,
                                     mc.cores=nthread,
                                     mc.cleanup = TRUE)}
    wm <- unlist(result_list)
  } else {
    wm <- apply(compute_outer(a, b, '-') * sc,
                2,
                function(x) which.min(abs(c - d) * (1 - sc) + x))
  }

  print(paste("Computation with Rccp and original implementation are the same: ",
              identical(wm, wm_cpp)))
  print(paste("Computation with Rccp_mc and original implementation are the same: ",
              identical(wm, wm_cpp_mc)))

  return(wm)


}
