
#ifndef DIP_FARRAY_UTILS_H
#define DIP_FARRAY_UTILS_H

#include "Rcpp.h"

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
SEXP dropDimension(SEXP x);

// [[Rcpp::export]]
int64_t prod2(SEXP x, bool na_rm = false);

// [[Rcpp::export]]
SEXP parseDots(Rcpp::Environment& env, bool eval);

// [[Rcpp::export]]
bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError = true);

// [[Rcpp::export]]
SEXPTYPE getSexpType(SEXP x);

// [[Rcpp::export]]
SEXP tik();

// [[Rcpp::export]]
SEXP tok(std::string msg, bool stop = false);

std::string as_dirpath(std::string x);

template <typename T>
inline std::vector<T> seq_len3(int64_t n){
  std::vector<T> re = std::vector<T>(n);
  T v = 1;
  for(auto it = re.begin(); it != re.end(); it++){
    *it = v++;
  }
  return re;
}

void setReIm(Rcpp::ComplexVector x, Rcpp::NumericVector v, bool is_real);

#endif // DIP_FARRAY_UTILS_H
