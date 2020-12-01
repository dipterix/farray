#ifndef DIP_FARRAY_CLASS_H
#define DIP_FARRAY_CLASS_H

#include <vector>
#include <string>
#include <Rcpp.h>


template <typename T>
class FArray {


public:

  // --------------------------- Constructor ------------------------------
  FArray(
    const std::string& rootPath,
    const std::vector<int64_t>& dim,
    const std::vector<int64_t>& bDim,
    const SEXPTYPE dType,
    const T na
  );

  ~FArray(){
    this->removeCachedIndex();
  }

  // --------------------------- properties ------------------------------

  // where is root directory
  std::string rootPath;
  // see Common.h
  std::vector<int64_t> dim;
  std::vector<int64_t> iDim;
  std::vector<int64_t> fDim;
  std::vector<int64_t> bDim;

  // data type
  SEXPTYPE dType;
  T na;

  // to cache parsed index, disabled if cacheKey == ""
  std::string cacheKey;
  std::vector<std::vector<int64_t>> cachedIndex;

  // --------------------------- members ------------------------------

  // Always store cachedIndex and cacheKey
  // Except: if cacheKey != "" and this.cacheKey == cacheKey, then return cachedIndex
  std::vector<std::vector<int64_t>> parseIndex(const SEXP listOrEnv, std::string cacheKey = "");
  void removeCachedIndex();

  // subset array using cachedIndex
  // if listOrEnvOrNil is R_NilValue and cachedIndex exists (length == ndim), use cachedIndex
  // if listOrEnvOrNil is not R_NilValue, call parseIndex and use cachedIndex
  void subsetArray(SEXP results, SEXP listOrEnvOrNil = R_NilValue);

};


#endif // DIP_FARRAY_CLASS_H
