#include "RFunctions.h"
#include <string>
#include <Rcpp.h>
using namespace Rcpp;


std::string Rf2_normalizePath(const std::string& path, const bool& mustWork) {
  Environment env = Environment::base_env();
  Function f = env["normalizePath"];
  std::string re = as<std::string>(f(path, "\\", mustWork));
  return re;
}

std::vector<int64_t> Rf2_uniqueIndex(const std::vector<int64_t>& x) {
  Environment env = Environment::base_env();
  Function f = env["unique"];
  std::vector<int64_t> re = as<std::vector<int64_t>>(f(x));
  return re;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
microbenchmark::microbenchmark({countIndexSet(a)}, {unique(a)}, {Rf2_uniqueIndex(a)})
*/
