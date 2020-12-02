#include <Rcpp.h>
#include "FArrayClass.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP farray_subset(
    const std::string rootPath, const std::vector<int64_t> dim,
    const std::vector<int64_t> bDim, const SEXPTYPE dType,
    SEXP listOrEnv
  ) {
  FArray<double> *arr = new FArray<double>(rootPath, dim, bDim, REALSXP, NA_REAL);
  arr->removeCachedIndex();
  arr->parseIndex(listOrEnv, "a");
  const std::vector<std::vector<int64_t>> idxSets = arr->cachedIndex;
  std::vector<int64_t> tDim = idxSets[3];
  int64_t expected_len = std::accumulate(tDim.begin(), tDim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
  SEXP res = PROTECT(Rf_allocVector(REALSXP, expected_len));

  arr->subsetArray(res);

  Rf_setAttrib(res, wrap("dim"), wrap(tDim));

  delete arr;
  UNPROTECT(1);
  // return wrap(idxSets);
  return res;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
a <- farray_subset('~/Desktop/filearray_data/', c(256, 200, 600, 84), c(16, 1, 1), 13L, list(NULL,1:20,1:60,1:4)); a
*/
