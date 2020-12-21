#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
SEXP addTo(SEXP x, SEXP y) {
  R_xlen_t lx = Rf_xlength(x);
  R_xlen_t ly = Rf_xlength(y);

  if(ly == 0 || lx == 0){
    return R_NilValue;
  }

  SEXPTYPE xType = TYPEOF(x);
  SEXPTYPE yType = TYPEOF(y);
  SEXP y_alt = y;
  int nprot = 0;

  if(xType != REALSXP) {
    stop("C++: `addInPlace`: x must be double.");
  }
  if(yType != REALSXP) {
    y_alt = PROTECT(Rcpp::r_cast<REALSXP>(y));
    nprot++;
  }

  R_xlen_t j = 0;
  double* p1 = REAL(x);
  double* p2 = REAL(y_alt);

  for(R_xlen_t i = 0; i < lx; i++){
    if (j == ly){
      j = 0;
    }

    *(p1 + i) += *(p2 + j);

    j++;
  }

  if(nprot > 0){
    UNPROTECT(nprot);
  }


  return R_NilValue;
}

/*** R
a <- rnorm(1e7)
microbenchmark::microbenchmark(
  {
    set_farray_threads(1)
    addTo(a, a)
  }, {
    set_farray_threads(4)
    addTo(a, a)
  }, {
    a+a
  }, times = 50
)

*/
