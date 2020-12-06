#include <cstring>
#include "rFunctions.h"
#include "common.h"
#include "binIo.h"
using namespace Rcpp;

bool Rf2_fileExists(const std::string& file) {
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Function f = env["file.exists"];
  SEXP e = f(file);
  return LOGICAL(e)[0];
}

SEXP ensurePartition(const std::string& file, int64_t length, SEXP na, int size){
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Function fileExists = env["file.exists"];
  Function fileSize = env["file.size"];

  SEXP e = fileExists(file);
  if(LOGICAL(e)[0]){
    e = fileSize(file);
    if(as<int64_t>(e) >= size* length) {
      return R_NilValue;
    }
  }

  RFileConn fcon;
  fcon.connect(file, "r+b", true);
  fcon.ensureLength(length, size, na);
  fcon.close();
  return(R_NilValue);
}

RFileConn::RFileConn() {
  this->file = "";
  this->mode = "rb";
  this->conn = R_NilValue;
  Rcpp::Environment env = Rcpp::Environment::base_env();
  this->env = env;
  this->ns = Rcpp::Environment::namespace_env("farray");
  this->raw_buffer = R_NilValue;
  this->locked = false;
};

size_t RFileConn::seek(int64_t where, const std::string& rw, const std::string& origin) {
  if(this->locked){
    stop("???");
  }
  this->locked = true;
  Function f = env["seek"];
  SEXP re = f(this->conn, Shield<SEXP>(wrap(where)), Shield<SEXP>(wrap(origin)), Shield<SEXP>(wrap(rw)));
  this->locked = false;
  return as<size_t>(re);
}

int64_t RFileConn::fileBytes(){
  int64_t re = 0;
  if(!this->exists()){ return (re); }
  // if(this->isValid()) {
  //   Function fl = this->env["flush"];
  //   fl(this->conn);
  // }
  Function fs = this->env["file.size"];
  SEXP len = fs(this->file);
  re = as<int64_t>(len);
  return(re);
}

void RFileConn::ensureLength(int64_t partition_length, int64_t size, SEXP na) {
  if(!this->exists()){
    Function fileCreate = this->env["file.create"];
    fileCreate(this->file);
  }
  int64_t current_bytes = this->fileBytes();
  int64_t expected_bytes = partition_length * size - current_bytes;
  int64_t buffer_bytes = getFArrayBlockSize(2);


  if(expected_bytes > 0){
    if(buffer_bytes < expected_bytes) {
      buffer_bytes = expected_bytes;
    }
    // open connection and write
    Function rep = this->env["rep"];
    Function write_bin = this->env["writeBin"];

    this->seek(0, "write", "end");

    SEXP buffer = PROTECT(rep(na, buffer_bytes / size));
    while(expected_bytes >= buffer_bytes) {
      write_bin(buffer, this->conn, size, "little");
      expected_bytes -= buffer_bytes;
    }

    if(expected_bytes > 0){
      UNPROTECT(1);
      buffer = PROTECT(rep(na, expected_bytes / size));
      write_bin(buffer, this->conn, size, "little");
    }
    this->seek(0, "write", "start");
    UNPROTECT(1);
  }
}

void RFileConn::writeRaw(Rbyte* buffer, size_t len) {
  // must make sure the length is ok
  if(this->raw_buffer == R_NilValue || Rf_xlength(this->raw_buffer) != len) {
    this->raw_buffer = Shield<SEXP>(Rf_allocVector(RAWSXP, len));
  }
  Rbyte* b = RAW(this->raw_buffer);
  for(size_t ii = 0; ii < len; ii++){
    *b++ = *buffer++;
  }
  Function f = env["writeBin"];
  f(this->raw_buffer, this->conn);
  this->seek(0, "write", "start");
}

bool RFileConn::exists() {
  Function f = env["file.exists"];
  SEXP e = f(this->file);
  return LOGICAL(e)[0];
}

void RFileConn::connect(const std::string& file, const std::string& mode, bool create) {
  if(this->isValid()) {
    this->close();
  }
  this->file = file;
  this->mode = mode;

  if(!this->exists() && create){
    // create file
    Function fileCreate = env["file.create"];
    fileCreate(wrap(file));
  }

  Function openFile = this->ns["open_conn"];
  Shield<SEXP>(this->conn =
    openFile(Rcpp::Shield<SEXP>(Rcpp::wrap(file)),
             Rcpp::Shield<SEXP>(Rcpp::wrap(mode)),
             Rcpp::Shield<SEXP>(Rcpp::wrap(true)),
             Rcpp::Shield<SEXP>(Rcpp::wrap("native.enc")),
             Rcpp::Shield<SEXP>(Rcpp::wrap(false)),
             Rcpp::Shield<SEXP>(Rcpp::wrap("default.enc")))
  );
};

bool RFileConn::isValid() {
  if((this->file).empty() || this->conn == R_NilValue) {
    return(false);
  }
  if(!this->exists()){
    return(false);
  }
  bool valid = false;
  try {
    Function f = env["isOpen"];
    SEXP v = f(conn, Shield<SEXP>(wrap("")));
    valid = LOGICAL(v)[0];
    return(valid);
  } catch(...){}
  return(false);
};

void RFileConn::close(){
  try{
    Function f = this->ns["close_conn"];
    f(this->conn);
  } catch (...) {}
  this->conn = R_NilValue;
};


// [[Rcpp::export]]
SEXP testt(std::string file){
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Function f = env["gc"];
  RFileConn* con = new RFileConn();

  Rcout << "connect\n";
  con->connect(file, "r+b");
  f();

  Rcout << "ensure\n";
  con->ensureLength(10, 8, wrap(NA_REAL));
  f();

  Rcout << "valid?\n";
  con->isValid();
  f();

  Rcout << "ensure 2\n";
  con->ensureLength(20, 8, wrap(NA_REAL));
  f();


  con->close();
  delete con;
  return R_NilValue;
}

/*** R
f <- tempfile()
conn <- testt(f)
# Rf2_closeFileConnection(conn)
# file(f, "r+b")

# writeSeq = function(start, value){
#   stopifnot( start >= 1L );
#   stopifnot( start+length(value)-1 <= nr*nc );
#   seek(con = fid, where = (start-1L)*size, rw = "write");
#
#   # Writing data of non-naitive size is slow in R. (Why?)
#   # This is solved by writing RAW data after using
#   # writeBin to convert it into memory vector.
#   if( ((size!=8) && (type=="double")) ||
#       ((size!=4) && (type=="integer")) ){
#     addwrite = function(value){
#       tmp = writeBin(
#         con = raw(),
#         object = caster(value),
#         size = size,
#         endian = "little");
#       filelock$lockedrun({
#         writeBin(con = fid, object = tmp);
#       });
#     }
#   } else {
#     addwrite = function(value){
#       filelock$lockedrun({
#         writeBin(
#           con = fid,
#           object = caster(value),
#           size = size,
#           endian = "little");
#       });
#     }
#   }
#
#   # Writing long vectors is currently NOT supported
#   # (as of R 3.2.2, 3.3.0).
#   # Thus write in pieces of 128 MB or less.
#   if(length(value)*as.numeric(size) < 134217728){
#     addwrite(value);
#   } else {
#     step1 = 134217728 %/% size;
#     mm = length(value);
#     nsteps = ceiling(mm/step1);
#     for( part in 1:nsteps ){ # part = 1
#       # cat( part, "of", nsteps, "\n");
#       fr = (part-1)*step1 + 1;
#       to = min(part*step1, mm);
#
#       addwrite(value[fr:to]);
#     }
#     rm(part, step1, mm, nsteps, fr, to);
#   }
#   # Instead of flush:
#   filelock$lockedrun({
#     seek(con = fid, where = 0, rw = "write");
#   });
#   return(invisible(.self));
# },
*/
