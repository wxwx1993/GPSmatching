#include <Rcpp.h>
using namespace Rcpp;

#ifdef _OPENMP
  #include <omp.h>
#else
  #define omp_get_num_threads()  1
  #define omp_get_thread_num()   0
  #define omp_get_max_threads()  1
  #define omp_get_thread_limit() 1
  #define omp_get_num_procs()    1
  #define omp_set_nested(a)   // empty statement to remove the call
  #define omp_get_wtime()        0
#endif

// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
IntegerVector compute_closest_wgps_helper(NumericVector a,
                                          NumericVector b,
                                          NumericVector cd,
                                          double sc,
                                          int nthread) {

  // a is the subset of data
  // b is the original data

  int size_a = a.size();
  int size_b = b.size();

  IntegerVector out(size_b);

  #if defined(_OPENMP)
    omp_set_num_threads(nthread);
    #pragma omp parallel for
  #endif
  for (int i = 0; i < size_b; ++i) {

    double tmp_val = 0;

    double subtract_val = fabs((b[i] - a[0])* sc);
    double min_val = subtract_val + cd[0];
    int min_index = 0;

    for (int j=1; j < size_a; ++j) {

      subtract_val = fabs((b[i] - a[j]) * sc);
      tmp_val =  subtract_val + cd[j];

      if (tmp_val < min_val){
        min_val = tmp_val;
        min_index = j;
      }
    }
    out[i] = min_index + 1;
  }
  return out;
}
