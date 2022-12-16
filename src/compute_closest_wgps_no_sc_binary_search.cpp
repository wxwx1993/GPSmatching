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
IntegerVector compute_closest_wgps_no_sc_binary_search(NumericVector a,
                                                NumericVector b,
                                                int nthread) {

  // a is the subset of data which are sorted.
  // b is the original data

  int size_a = a.size();
  int size_b = b.size();

  IntegerVector out(size_b);

#if defined(_OPENMP)
  omp_set_num_threads(nthread);
#pragma omp parallel for
#endif
  for(int i = 0; i < size_b; ++i) {

    double min_val = a[0] - b[i];
    if(min_val < 0){ min_val *= -1;}
    int min_index = 0;

    int left = 0;
    int right = size_a - 1;

    while (left <= right) {
      int mid = (left + right) / 2;
      double tmp_val = a[mid] - b[i];
      if (tmp_val < 0){tmp_val *= -1;}
      if (tmp_val < min_val){
        min_val = tmp_val;
        min_index = mid;
      }
      if (b[i] < a[mid]){
        right = mid - 1;
      } else {
        left = mid + 1;
      }
    }
   out[i] = min_index + 1;
  }
  return out;
}

