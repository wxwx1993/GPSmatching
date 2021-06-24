#include <Rcpp.h>
#include <omp.h>
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
IntegerVector compute_closest_wgps_helper(NumericVector a,
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


  #if defined(_OPENMP)
  int nthread = omp_get_max_threads();
  omp_set_num_threads(nthread);
  #pragma omp parallel for
  #endif
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
