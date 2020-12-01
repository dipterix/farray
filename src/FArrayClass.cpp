#include "FArrayClass.hpp"
#include "RFunctions.h"
#include "ArrayIndex.h"
// --------------------------- Constructor ------------------------------

template <typename T>
FArray<T>::FArray(
  const std::string& rootPath,
  const std::vector<int64_t>& dim,
  const std::vector<int64_t>& bDim,
  const SEXPTYPE dType,
  const T na
): dim(dim), bDim(bDim), dType(dType){
  this->rootPath = Rf2_normalizePath(rootPath);

  // calculate iDim and fDim
  int ndims = dim.size();
  int bDimSize = bDim.size();
  int fDimSize = ndims - bDimSize;
  if(fDimSize <= 0){
    Rcpp::stop("C++: `FArray-Class`: total dimension size should be greater than block dimension");
  }
  this->iDim = std::vector<int64_t>(bDimSize);
  this->fDim = std::vector<int64_t>(fDimSize);
  for( int dd = 0 ; dd < bDimSize; dd++ ){
    int rem = dim[dd] % bDim[dd];
    if(rem == 0){
      this->iDim[dd] = dim[dd];
    } else {
      this->iDim[dd] = dim[dd] - rem + bDim[dd];
    }
  }
  for(int dd = 0; dd < fDimSize; dd++){
    this->fDim[dd] = dim[dd + bDimSize];
  }


  this->cacheKey = "";
  this->cachedIndex = std::vector<std::vector<int64_t>>();
  this->na = na;
}

// --------------------------- member functions ------------------------------

// Always store cachedIndex and cacheKey
// Except: if cacheKey != "" and this.cacheKey == cacheKey, then return cachedIndex
template <typename T>
std::vector<std::vector<int64_t>> FArray<T>::parseIndex(const SEXP listOrEnv, std::string cacheKey) {
  if(cacheKey.compare("") || !cacheKey.compare(this->cacheKey) || this->cachedIndex.size() != 3){
    this->cachedIndex = listOrEnv2Idx2(listOrEnv, this->dim, this->bDim);
  }
  return this->cachedIndex;
};

template <typename T>
void FArray<T>::removeCachedIndex(){
  this->cacheKey = "";
  this->cachedIndex = std::vector<std::vector<int64_t>>();
};

// subset array using cachedIndex
// if listOrEnvOrNil is R_NilValue and cachedIndex exists (length == ndim), use cachedIndex
// if listOrEnvOrNil is not R_NilValue, call parseIndex and use cachedIndex
template <typename T>
void FArray<T>::subsetArray(SEXP results, SEXP listOrEnvOrNil){
  // No check! (no error handling, make sure cachedIndex exists or listOrEnvOrNil is not null)
  if( listOrEnvOrNil != R_NilValue ){
    this->parseIndex(listOrEnvOrNil, "");
  }

  // check if SEXP is consistent
  if(TYPEOF(results) != this->dType){
    Rcpp::stop("C++ `FArray-Class`: SEXPTYPE does not match");
  }
  // check if cachedIndex is valid
  if(this->cachedIndex.size() != 4){
    Rcpp::stop("C++ `FArray-Class`: cachedIndex is not a vector(4)");
  }
  // Index:
  // 1: sub index within block
  // 2: block index
  // 3. file index
  // 4. target dimension

  // get index
  std::set<int64_t> subBlockIdx   = this->cachedIndex[0];
  std::set<int64_t> blockIdx      = this->cachedIndex[1];
  std::set<int64_t> fileIdx       = this->cachedIndex[2];
  std::set<int64_t> tDim          = this->cachedIndex[3];
  // check if results length is valid
  int64_t tLen = Rf_xlength(results);
  if(results != std::accumulate(tDim.begin(), tDim.end(), INTEGER64_ONE, std::multiplies<int64_t>())){
    Rcpp::stop("C++: `FArray-Class`: result length does not match.");
  }

  // Find unique block sets
  std::set<int64_t> uniqueBlockSet = uniqueIndexSet(blockIdx);
  // buffer size
  int64_t bLen = std::accumulate(this->bDim.begin(), this->bDim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t bufferSize = bLen;

  // reserved for OPENMP
  {
    // Schedule buffers
    T buffer[bufferSize]; // private

    // Read data to results, openmp for
    for(fii = 0; fii < fileIdx.size(); fii++){

    }

  }





  // Slice!
  std::vector<T> slice = std::vector<T>();

  // clean up
  if( (this->cacheKey).compare("") ){
    this->removeCachedIndex();
  }
};


