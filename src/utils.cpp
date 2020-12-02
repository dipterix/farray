#include "utils.h"

#include <chrono>
#include <string>
#include "common.h"
using namespace Rcpp;

static std::chrono::time_point<std::chrono::high_resolution_clock> _timer;
static std::vector<double> _times = std::vector<double>(0);
static std::vector<std::string> _timer_msg = std::vector<std::string>(0);
static bool _timer_enabled = false;


std::string as_dirpath(std::string x){
  std::string re = "./";
  if(x.size() > 0){
    std::string ending = "/";
    if(std::equal(ending.rbegin(), ending.rend(), x.rbegin())){
      re = x;
    } else {
      re = x + ending;
    }
  }
  return re;
}

SEXP tik(){
  if(!_timer_enabled){
    _timer = std::chrono::high_resolution_clock::now();
    _times.clear();
    _timer_msg.clear();
    _timer_enabled = true;
    return wrap(true);
  }
  return wrap(false);
}

SEXP tok(std::string msg, bool stop){
  if(!_timer_enabled){ return R_NilValue; }
  std::chrono::time_point<std::chrono::high_resolution_clock> now = std::chrono::high_resolution_clock::now();
  uint64_t delta = std::chrono::duration_cast<std::chrono::nanoseconds>(now - _timer).count();
  _times.push_back((double)(delta) / 1000000.0);
  _timer_msg.push_back(msg);
  if(!stop){
    return wrap(delta);
  } else {
    Rcpp::List re = Rcpp::List::create(
      _["messages"] = wrap(_timer_msg),
      _["time"] = wrap(_times)
    );
    _timer_enabled = false;
    return wrap(re);
  }
}


SEXP dropDimension(SEXP x){
  SEXP dim = Rf_getAttrib(x, wrap("dim"));
  if(dim == R_NilValue){
    return x;
  }
  SEXP new_dim;
  R_xlen_t ndims = Rf_xlength(dim);
  R_xlen_t xlen = Rf_xlength(x);
  if(ndims == 0){
    new_dim = R_NilValue;
    Rf_setAttrib(x, wrap("dim"), new_dim);
    return x;
  }
  if(xlen == 0){
    return x;
  }

  R_xlen_t ii;

  new_dim = PROTECT(Rf_allocVector(TYPEOF(dim), ndims));

  switch(TYPEOF(dim)){
  case INTSXP: {
    int *ptr_orig = INTEGER(dim);
    int *ptr_new = INTEGER(new_dim);
    for(ii = 0; ptr_orig != INTEGER(dim) + ndims; ptr_orig++ ){
      if(*ptr_orig > 1){
        *ptr_new++ = *ptr_orig;
        ii++;
      }
    }
    break;
  }
  case REALSXP: {
    double *ptr_orig = REAL(dim);
    double *ptr_new = REAL(new_dim);
    for(ii = 0; ptr_orig != REAL(dim) + ndims; ptr_orig++ ){
      if(*ptr_orig > 1){
        *ptr_new++ = *ptr_orig;
        ii++;
      }
    }
    break;
  }
  default:
    stop("unknown dimension storage type");
  }
  if(ii == ndims){} else if(ii >= 2){
    SETLENGTH(new_dim, ii);

    Rf_setAttrib(x, wrap("dim"), new_dim);
  } else {
    Rf_setAttrib(x, wrap("dim"), R_NilValue);
  }

  UNPROTECT(1);
  return x;
}


int64_t prod2(SEXP x, bool na_rm){
  SEXP x_alt = x;

  int n_protected = 0;

  if(TYPEOF(x_alt) != REALSXP){
    x_alt = PROTECT(Rf_coerceVector(x_alt, REALSXP));
    n_protected++;
  }
  int64_t res = 1;
  R_xlen_t xlen = Rf_xlength(x) - 1;
  for(; xlen >= 0; xlen-- ){
    int64_t tmp = REAL(x_alt)[xlen];
    if(tmp == NA_REAL || tmp == NA_INTEGER64){
      if(!na_rm){
        res = NA_INTEGER64;
        break;
      }
    } else {
      res *= REAL(x_alt)[xlen];
    }
  }

  if( n_protected > 0 ){
    UNPROTECT(n_protected);
  }

  return res;
}

SEXP parseDots(Environment& env, bool eval){

  SEXP dots = Rf_findVarInFrame(env, R_DotsSymbol);

  List res = List::create();
  R_xlen_t idx_size = 0;
  SEXP el;

  for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots) ){
    el = CAR(dots);

    // el might be promise SEXP, if so, evaluate
    if( TYPEOF(el) == PROMSXP ){
      if(eval){
        el = Rf_eval( PREXPR(el), PRENV( el ));
      } else {
        el = PREXPR(el);
      }
    }

    res.push_back( el );
    idx_size++;

  }

  SEXP tl = PROTECT(Rf_allocVector(REALSXP, idx_size));
  SEXP ty = PROTECT(Rf_allocVector(INTSXP, idx_size));

  for(R_xlen_t ii = 0; ii < idx_size; ii++ ){
    el = res[ii];
    if(el == R_MissingArg ){
      REAL(tl)[ii] = -1;
    } else if (TYPEOF(el) == PROMSXP){
      REAL(tl)[ii] = -2;
    } else {
      REAL(tl)[ii] = Rf_xlength(el);
    }
    INTEGER(ty)[ii] = TYPEOF(el);
  }

  Rf_setAttrib(res, wrap("element_length"), tl);
  Rf_setAttrib(res, wrap("element_type"), ty);

  UNPROTECT(2);

  return res;
}


bool stopIfNot(const bool isValid, const std::string& message, bool stopIfError){
  if(!isValid){
    if( stopIfError ){
      Rcpp::stop(message);
    } else {
      Rcpp::warning(message);
    }
    return false;
  }
  return true;
}

SEXPTYPE getSexpType(SEXP x){
  return TYPEOF(x);
}


void setReIm(ComplexVector x, NumericVector v, bool is_real){
  if(x.size() != v.size()){
    stop("Cannot copy values: length mismatch");
  }
  NumericVector::iterator ptr_v = v.begin();
  if(is_real) {
    for(ComplexVector::iterator ptr_x = x.begin(); ptr_x != x.end(); ){
      (*ptr_x++).r = *ptr_v++;
    }
  } else {
    for(ComplexVector::iterator ptr_x = x.begin(); ptr_x != x.end(); ){
      (*ptr_x++).i = *ptr_v++;
    }
  }
}

/*** R
f <- function(...){
  parseDots(environment(), F)
}
a = f(rnorm(10),)
as.call(c(list(quote(x)), a))


*/
