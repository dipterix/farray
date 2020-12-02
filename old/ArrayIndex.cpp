#include "ArrayIndex.h"
#include "Common.h"

using namespace Rcpp;

// location (list) to index (start from 1)
std::vector<int64_t> loc2idx(const std::vector<std::vector<int64_t>>& locations, const std::vector<int64_t>& dim){
  int64_t dimSize = dim.size();
  if( dimSize != locations.size() ){
    stop("C++ `loc2idx`: Dimension input wrong.");
  }

  std::vector<int64_t> sub_dim(dimSize);
  for(int64_t dd = 0; dd < dimSize; dd++){
    std::vector<int64_t> subloc = locations[dd];
    sub_dim[dd] = subloc.size();
  }
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());

  // Inflate indexes and add them
  R_xlen_t ii = 0, jj = 0;
  R_xlen_t inflate = 1, margin = 0;
  R_xlen_t neach = 1;

  std::vector<int64_t> re = std::vector<int64_t>(sub_size, 0);
  std::vector<int64_t>::iterator ptr_loc, ptr_re;

  for(ii = 0; ii < dimSize; ii++ ){

    margin = dim[ii];

    // Ger slice index
    std::vector<int64_t> loc = locations[ii];

    // Remember Assign invalid indexes to NA
    ptr_loc = loc.begin();
    ptr_re = re.begin();
    for( ; ptr_re != re.end(); ){
      if( ptr_loc == loc.end() ){
        ptr_loc = loc.begin();
      }
      for(jj = 0; jj < neach; jj++){
        // if re[...] is not NA
        if(*ptr_re != NA_INTEGER64 && *ptr_loc != NA_INTEGER64 && *ptr_loc > 0 && *ptr_loc <= margin){
          // not NA
          *ptr_re += (*ptr_loc - 1) * inflate;
        } else {
          *ptr_re = NA_INTEGER64;
        }
        ptr_re++;
      }
      if(ptr_loc != loc.end()){ ptr_loc++; }
    }
    // Prepare for next loop
    inflate = inflate * margin;
    neach = neach * loc.size();

  }
  return(re);
}

std::vector<std::vector<int64_t>> loc2idx2(
    const std::vector<std::vector<int64_t>>& locations,
    const std::vector<int64_t>& iDim,
    const std::vector<int64_t>& bDim
  ){

  // Check whether parent_dim matches with location index size - validation
  int64_t iDimSize = iDim.size();
  int64_t bDimSize = bDim.size();

  // unlikely to happen
  if( iDimSize != locations.size() || bDimSize != iDimSize ){
    stop("C++ `loc2idx2`: Dimension input wrong.");
  }

  // Get sub-dimension for location indexes (the dimension of returned value)
  std::vector<int64_t> sub_dim(iDimSize);
  std::vector<int64_t> iBDim(iDimSize);
  std::vector<std::vector<int64_t>> blockLocation(iDimSize);
  std::vector<std::vector<int64_t>> blockSubLocation(iDimSize);
  std::vector<int64_t>::iterator ptr1, ptr2, ptr3;
  for(int64_t dd = 0; dd < iDimSize; dd++){
    std::vector<int64_t> subloc = locations[dd];
    sub_dim[dd] = subloc.size();
    // calculate (subloc - 1) %% bDim[dd]
    std::vector<int64_t> bQuot = std::vector<int64_t>(subloc.size());
    std::vector<int64_t> bRem = std::vector<int64_t>(subloc.size());
    int64_t bMargin = bDim[dd];
    int64_t iMargin = iDim[dd];
    iBDim[dd] = iMargin / bMargin;
    for(
      ptr1 = subloc.begin(), ptr2 = bQuot.begin(), ptr3 = bRem.begin();
      ptr1 != subloc.end();
      ptr1++, ptr2++, ptr3++
    ) {
      if(*ptr1 == NA_REAL || *ptr1 == NA_INTEGER64 || *ptr1 == NA_INTEGER){
        *ptr3 = NA_INTEGER64;
        *ptr2 = NA_INTEGER64;
      } else if(*ptr1 > iMargin || *ptr1 < 1){
        stop("C++ `loc2idx2`: invalid subscripts");
      } else {
        *ptr3 = (*ptr1 - 1) % bMargin;
        *ptr2 = (*ptr1 - *ptr3 - 1) / bMargin;
      }
    }
    blockLocation[dd] = bQuot;
    blockSubLocation[dd] = bRem;
  }

  // Total length to return
  int64_t sub_size = std::accumulate(sub_dim.begin(), sub_dim.end(), INTEGER64_ONE, std::multiplies<int64_t>());

  // Generate integer vector to be returned and assign dimension
  // <block index, sub-block index>
  // expand using iBDim and dBim
  std::vector<int64_t> blockIdx(sub_size, 0);
  std::vector<int64_t> subBlockIdx(sub_size, 0);
  std::vector<int64_t>::iterator ptr_blockIdx;
  std::vector<int64_t>::iterator ptr_subBlockIdx;

  if( sub_size == 0 ){
    std::vector<std::vector<int64_t>> re = std::vector<std::vector<int64_t>>(4);
    return (re);
  }

  // Inflate indexes and add them
  R_xlen_t ii = 0, jj = 0;
  R_xlen_t bInflate = 1, iBInflate= 1;
  R_xlen_t bMargin = 0, iBMargin = 0;
  R_xlen_t neach = 1;

  for(ii = 0; ii < iDimSize; ii++ ){

    bMargin = bDim[ii];
    iBMargin = iBDim[ii];

    // Ger slice index
    std::vector<int64_t> bQuot = blockLocation[ii];
    std::vector<int64_t> bRem = blockSubLocation[ii];

    // Remember Assign invalid indexes to NA
    ptr1 = bQuot.begin();
    ptr2 = bRem.begin();
    ptr_blockIdx = blockIdx.begin();
    ptr_subBlockIdx = subBlockIdx.begin();
    for( ; ptr_blockIdx != blockIdx.end(); ){
      if(ptr1 == bQuot.end() || ptr2 == bRem.end()){
        ptr1 = bQuot.begin();
        ptr2 = bRem.begin();
      }
      for(jj = 0; jj < neach; jj++){
        // if re[...] is not NA
        if(*ptr_blockIdx != NA_INTEGER64 && *ptr1 != NA_INTEGER64 && *ptr1 >= 0 && *ptr1 < iBMargin){
          // not NA
          *ptr_blockIdx += (*ptr1) * iBInflate;
        } else {
          *ptr_blockIdx = NA_INTEGER64;
        }
        ptr_blockIdx++;

        if(*ptr_subBlockIdx != NA_INTEGER64 && *ptr2 != NA_INTEGER64 && *ptr2 >= 0 && *ptr2 < bMargin){
          // not NA
          *ptr_subBlockIdx += (*ptr2) * bInflate;
        } else {
          *ptr_subBlockIdx = NA_INTEGER64;
        }
        ptr_subBlockIdx++;
      }
      if(ptr1 != bQuot.end()){ ptr1++; }
      if(ptr2 != bRem.end()){ ptr2++; }
    }
    // Prepare for next loop
    bInflate = bInflate * bMargin;
    iBInflate = iBInflate * iBMargin;
    neach = neach * bQuot.size();

  }
  std::vector<std::vector<int64_t>> re = std::vector<std::vector<int64_t>>(4);
  re[0] = subBlockIdx;
  re[1] = blockIdx;
  return(re);
}


std::vector<std::vector<int64_t>> listOrEnv2Idx2(SEXP listOrEnv, std::vector<int64_t> dim, std::vector<int64_t> bDim) {
  // calculate iDim
  int ndims = dim.size();
  int bDimSize = bDim.size();
  int fDimSize = ndims - bDimSize;
  if(bDimSize >= ndims){
    stop("C++: `listOrEnv2Idx2`: total dimension size should be greater than block dimension");
  }
  std::vector<int64_t> iDim = std::vector<int64_t>(bDimSize);
  std::vector<int64_t> fDim = std::vector<int64_t>(fDimSize);
  for( int dd = 0 ; dd < bDimSize; dd++ ){
    int rem = dim[dd] % bDim[dd];
    if(rem == 0){
      iDim[dd] = dim[dd];
    } else {
      iDim[dd] = dim[dd] - rem + bDim[dd];
    }
  }
  for(int dd = 0; dd < fDimSize; dd++){
    fDim[dd] = dim[dd + bDimSize];
  }

  // Make locations for each partition
  std::vector<std::vector<int64_t>> iLoc = std::vector<std::vector<int64_t>>(bDimSize);
  std::vector<std::vector<int64_t>> fLoc = std::vector<std::vector<int64_t>>(ndims - bDimSize);
  std::vector<int64_t> tDim = std::vector<int64_t>(ndims);
  int idx_size;
  int n_protected = 0;
  switch(TYPEOF(listOrEnv)) {
  case ENVSXP: {
    SEXP dots = Rf_findVarInFrame(listOrEnv, R_DotsSymbol);
    for(idx_size = 0; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots), idx_size++ ){
      if(idx_size >= ndims){
        stop("Incorrect subscript dimensions, required: 0, 1, ndim.");
      }
      SEXP el = PROTECT(CAR(dots));
      n_protected++;

      // el might be promise SEXP, if so, evaluate
      if ( TYPEOF(el) == PROMSXP ){
        // This is a promise, need to evaluate
        el = Rf_eval( PREXPR(el), PRENV( el ));
      }
      // current margin size
      int64_t di = dim[ idx_size ];
      std::vector<int64_t> idx;
      // Check if el is missing value, this means seq_len(di)
      if( el == R_NilValue || el == R_MissingArg ){
        idx = std::vector<int64_t>(di);
        std::iota(idx.begin(), idx.end(), INTEGER64_ONE);
      } else {
        // check out if bound?
        NumericVector el_vec = as<NumericVector>(el);
        if(is_true( any( !(is_na(el_vec) | ((el_vec >= 1) & (el_vec <= di))) ) )){
          stop("C++ `listOrEnv2Idx2`: only positive subscript is allowed, check if subscript is out of bounds");
        }
        idx = as<std::vector<int64_t>>(el);
      }
      if(idx_size < bDimSize){
        iLoc[idx_size] = idx;
      } else {
        fLoc[idx_size - bDimSize] = idx;
      }
      tDim[idx_size] = idx.size();
    }
    break;
  }
  case VECSXP:
    if(Rf_xlength(listOrEnv) != ndims) {
      stop("Incorrect subscript dimensions, required: 0, 1, ndim.");
    }
    for(idx_size = 0; idx_size < ndims; idx_size++ ){
      SEXP el = PROTECT(VECTOR_ELT(listOrEnv, idx_size));
      n_protected++;
      // current margin size
      int64_t di = dim[ idx_size ];
      std::vector<int64_t> idx;
      // Check if el is missing value, this means seq_len(di)
      if( el == R_NilValue || el == R_MissingArg ){
        idx = std::vector<int64_t>(di);
        std::iota(idx.begin(), idx.end(), INTEGER64_ONE);
      } else {
        NumericVector el_vec = as<NumericVector>(el);
        if(is_true( any( !(is_na(el_vec) | ((el_vec >= 1) & (el_vec <= di))) ) )){
          stop("C++ `listOrEnv2Idx2`: only positive subscript is allowed, check if subscript is out of bounds");
        }
        idx = as<std::vector<int64_t>>(el);
      }
      if(idx_size < bDimSize){
        iLoc[idx_size] = idx;
      } else {
        fLoc[idx_size - bDimSize] = idx;
      }
      tDim[idx_size] = idx.size();
    }
    break;
  default:
    stop("Input `listOrEnv` must be either a list of indices or an environment");
  }
  if(n_protected > 0){
    UNPROTECT(n_protected);
  }

  // Make index
  std::vector<std::vector<int64_t>> indexSet = loc2idx2( iLoc, iDim, bDim );
  indexSet[2] = loc2idx(fLoc, fDim);
  indexSet[3] = tDim;
  return(indexSet);
}


std::vector<int64_t> uniqueIndexSet(const std::vector<int64_t>& idx){
  std::vector<int64_t> cp;
  std::unique_copy(idx.begin(), idx.end(), std::back_inserter(cp));
  return cp;
}


/*** R
a <- sample(1e7, 1e7, T)
# uniqueIndexSet = farray:::uniqueIndexSet
microbenchmark::microbenchmark({
  uniqueIndexSet2(a)
}, unique(a), times = 4)

# farray:::listOrEnv2Idx2(list(
#   1:3,
#   2:4,
#   1:4
# ), c(10, 11,1), c(2,2))
#
# loc2idx(list(1:3, 1:3), c(3,3))
*/
