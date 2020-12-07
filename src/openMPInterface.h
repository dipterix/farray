#ifndef FARRAY_OPENMP_H
#define FARRAY_OPENMP_H

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]

#ifdef _OPENMP
#include <omp.h>
#define FARRAY_HAS_OPENMP true
#else
#define omp_get_thread_num() 0
#define omp_get_max_threads() 1
#define FARRAY_HAS_OPENMP false
#endif

#include <Rcpp.h>




// [[Rcpp::export]]
int getFArrayThread(bool max = false);

// [[Rcpp::export]]
int setFArrayThread(int n, SEXP reset_after_fork = R_NilValue);

// [[Rcpp::export]]
bool hasOpenMP();

// [[Rcpp::init]]
int detectForked(DllInfo *dll);

#endif  // OPEN_MP_HELPER_H
