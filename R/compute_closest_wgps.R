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
compute_closest_wgps <- function(a, b, c, d, sc){

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


  Rcpp::cppFunction('IntegerVector compute_closest_wgps_helper(NumericVector a,
                                          NumericVector b,
                                          NumericVector cd,
                                          double sc) {

    // a is the subset of data
    // b is the original data

    //TODO: stop crashes due to type of compilers. I commented it out.
    //if (sc < 0 || sc > 1) stop("Scale (sc) should be in [0,1] range.");

    int size_a = a.size();
    int size_b = b.size();
    int min_index = 0;
    double min_val = 0;
    double tmp_val = 0;
    double subtract_val = 0;

    NumericVector tmp(size_a);
    IntegerVector out(size_b);

    for(int i = 0; i < size_b; ++i) {
      for(int j=0; j < size_a; ++j) {

        subtract_val = (b[i]-a[j])*sc;

        if (subtract_val < 0){subtract_val *= -1;}

        tmp_val =  subtract_val + cd[j];

        if (j==0){
          min_val = tmp_val;
          min_index = j;
          continue;
        }

        if (tmp_val < min_val){
          min_val = tmp_val;
          min_index = j;
        }
      }

      out[i] = min_index + 1;
    }
    return out;
}
')





   c_minus_d <- abs(c-d)*(1-sc)
   wm <- compute_closest_wgps_helper(a, b, c_minus_d, sc)

   return(wm)
}
