#ifndef DIP_FARRAY_INDEX_H
#define DIP_FARRAY_INDEX_H

#include <vector>
#include <Rcpp.h>

// [[Rcpp::export]]
std::vector<int64_t> loc2idx(const std::vector<std::vector<int64_t>>& locations, const std::vector<int64_t>& dim);

std::vector<std::vector<int64_t>> loc2idx2(
    const std::vector<std::vector<int64_t>>& locations,
    const std::vector<int64_t>& iDim,
    const std::vector<int64_t>& bDim
);

// [[Rcpp::export]]
std::vector<std::vector<int64_t>> listOrEnv2Idx2(SEXP listOrEnv, std::vector<int64_t> dim, std::vector<int64_t> bDim);

// [[Rcpp::export]]
std::vector<int64_t> uniqueIndexSet(const std::vector<int64_t>& idx);


#endif // DIP_FARRAY_INDEX_H
