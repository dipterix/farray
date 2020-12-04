#include <cstdio>
#include <Rcpp.h>
#include "common.h"
#include "binIo.h"
using namespace Rcpp;



// [[Rcpp::export]]
int64_t cpp_writeBin2(std::string file, SEXP data, int64_t skip = 0) {
  // FILE* conn, char* buffer, int64_t n, int size, int64_t skip, bool check_length

  BinaryFileConn conn = BinaryFileConn(file, true);

  if(!conn.isValid()){
    stop("Cannot open file for writing");
  }

  // index0 starts from 0
  int64_t dLen = Rf_xlength(data);


  try {
    switch(TYPEOF(data)) {
    case(LGLSXP):
      cpp_writeBin<int>(conn.conn, LOGICAL(data), dLen, NA_LOGICAL, skip);
      break;
    case(RAWSXP):
      cpp_writeBin<char>(conn.conn, (char*)RAW(data), dLen, 0, skip);
      break;
    case(REALSXP):
      cpp_writeBin<double>(conn.conn, REAL(data), dLen, NA_REAL, skip);
      break;
    case(INTSXP):
      cpp_writeBin<int>(conn.conn, INTEGER(data), dLen, NA_INTEGER, skip);
      break;
    default:
      Rcpp::warning("C++: `cpp_writeBin2`: data type is not supported.");
      break;
    }
  } catch (...) {}

  return dLen;
}



/*** R
f <- tempfile()
# writeBin(0.1, f)
# std::string file, std::vector<double> data, int element_size,
# std::vector<int64_t> index0, int64_t skip = 0,
# double na = NA_REAL)
cpp_writeBin2(f, 10:1, 10)

file.exists(f)
file.size(f)
readBin(f, 'int', n = 100, size = 4L, endian = 'little')


x <- rnorm(1e6);
f1 <- tempfile()
f2 <- tempfile()
microbenchmark::microbenchmark(
  {
    cpp_writeBin2(f1, x)
  },{
    writeBin(x, f2, 8L, endian = 'little')
  }, times = 3
)

*/
