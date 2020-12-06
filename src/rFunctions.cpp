#include "rFunctions.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP Rf2_fileConnection(const std::string& file, const std::string& mode) {
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Rcpp::Function f = env["file"];
  SEXP re = f(Rcpp::Shield<SEXP>(Rcpp::wrap(file)),
              Rcpp::Shield<SEXP>(Rcpp::wrap(mode)),
              Rcpp::Shield<SEXP>(Rcpp::wrap(true)),
              Rcpp::Shield<SEXP>(Rcpp::wrap("native.enc")),
              Rcpp::Shield<SEXP>(Rcpp::wrap(false)),
              Rcpp::Shield<SEXP>(Rcpp::wrap("default.enc")));
  return re;
}

// [[Rcpp::export]]
SEXP Rf2_closeFileConnection(SEXP conn) {
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Rcpp::Function f = env["close"];
  return f(conn);
}



/*** R
f <- tempfile()
conn <- Rf2_fileConnection(f, "w+b")
Rf2_closeFileConnection(conn)
# file(f, "r+b")

writeSeq = function(start, value){
  stopifnot( start >= 1L );
  stopifnot( start+length(value)-1 <= nr*nc );
  seek(con = fid, where = (start-1L)*size, rw = "write");

  # Writing data of non-naitive size is slow in R. (Why?)
  # This is solved by writing RAW data after using
  # writeBin to convert it into memory vector.
  if( ((size!=8) && (type=="double")) ||
      ((size!=4) && (type=="integer")) ){
    addwrite = function(value){
      tmp = writeBin(
        con = raw(),
        object = caster(value),
        size = size,
        endian = "little");
      filelock$lockedrun({
        writeBin(con = fid, object = tmp);
      });
    }
  } else {
    addwrite = function(value){
      filelock$lockedrun({
        writeBin(
          con = fid,
          object = caster(value),
          size = size,
          endian = "little");
      });
    }
  }

  # Writing long vectors is currently NOT supported
  # (as of R 3.2.2, 3.3.0).
  # Thus write in pieces of 128 MB or less.
  if(length(value)*as.numeric(size) < 134217728){
    addwrite(value);
  } else {
    step1 = 134217728 %/% size;
    mm = length(value);
    nsteps = ceiling(mm/step1);
    for( part in 1:nsteps ){ # part = 1
      # cat( part, "of", nsteps, "\n");
      fr = (part-1)*step1 + 1;
      to = min(part*step1, mm);

      addwrite(value[fr:to]);
    }
    rm(part, step1, mm, nsteps, fr, to);
  }
  # Instead of flush:
  filelock$lockedrun({
    seek(con = fid, where = 0, rw = "write");
  });
  return(invisible(.self));
},
*/
