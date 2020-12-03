#include <cstdio>
#include "common.h"
#include "binIo.h"
using namespace Rcpp;

// designed for file matrix
SEXP r_readBin(std::string con, int64_t n, int size){
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Rcpp::Function f = env["readBin"];
  SEXP re = f(Rcpp::Shield<SEXP>(Rcpp::wrap(con)), Rcpp::Shield<SEXP>(Rcpp::wrap("raw")), Rcpp::Shield<SEXP>(Rcpp::wrap(n * size)),
              Rcpp::Shield<SEXP>(Rcpp::wrap(1)), Rcpp::Shield<SEXP>(Rcpp::wrap(true)), Rcpp::Shield<SEXP>(Rcpp::wrap("little")));
  return re;
}

// con: file path
// buffer: char[n] (size must be at least n)
// n: number of elements to read
// size: R size of element: double is 8, int is 4...
int64_t cpp_readBin(FILE* conn, char* buffer, int64_t n,
                    int size, int64_t skip, bool check_length){
  // char* buffer = new char[n * size];
  // std::ifstream input( con, std::ios::binary );
  int64_t fsize = 1;
  int64_t n_byte = n * size;
  int64_t skip_byte = skip * size;
  char* ptr = buffer;
  try{
    // input.setf(std::ios::ios_base::skipws);
    // std::filebuf* pbuf = input.rdbuf();
    if(check_length){
      // fsize = pbuf->pubseekoff (-skip * size, input.end, input.beg);
      fseek(conn, 0, SEEK_END);
      fsize = ftell(conn) - skip_byte;
      if(fsize < size){
        n_byte = 0;
      } else {
        if(fsize < n_byte){ n_byte = fsize; }
        // pbuf->pubseekpos (skip * size, input.beg);
        // pbuf->sgetn (buffer, n_byte);
        fseek(conn, skip_byte, SEEK_SET);

        while(n_byte >= FARRAY_BUFFERSIZE && fsize > 0){
          // fsize not used to suppress warnings
          fsize = std::fread(ptr, 1, FARRAY_BUFFERSIZE, conn);
          n_byte -= FARRAY_BUFFERSIZE;
          ptr += FARRAY_BUFFERSIZE;
        }
        if(n_byte > 0){
          fsize = std::fread(ptr, 1, n_byte, conn);
        }
      }
    } else {
      // pbuf->pubseekpos (skip * size, input.beg);
      // pbuf->sgetn (buffer, n_byte);
      fseek(conn, skip_byte, SEEK_SET);
      // // fsize not used to suppress warnings
      // fsize = std::fread(buffer, 1, n_byte, conn);

      while(n_byte >= FARRAY_BUFFERSIZE && fsize > 0){
        // fsize not used to suppress warnings
        fsize = std::fread(ptr, 1, FARRAY_BUFFERSIZE, conn);
        n_byte -= FARRAY_BUFFERSIZE;
        ptr += FARRAY_BUFFERSIZE;
      }
      if(n_byte > 0){
        fsize = std::fread(ptr, 1, n_byte, conn);
      }
    }
  } catch (...) {
    n_byte = 0;
  }
  ptr = NULL;
  // input.close();
  return n_byte;
}


int64_t cpp_fileLength(const std::string& con){
  int64_t fsize = 0;
  // std::ifstream input( con, std::ios::binary );
  FILE* conn = fopen( con.c_str(), "rb" );

  try{
    // std::filebuf* pbuf = input.rdbuf();
    // fsize = pbuf->pubseekoff (0,input.end,input.beg);
    fseek(conn, 0, SEEK_END);
    fsize = ftell(conn);
  } catch(...){}
  // input.close();
  if( conn != NULL ){
    fclose(conn);
  }

  return fsize;
}

bool fileExists(const std::string& con){
  Environment env = Environment::base_env();
  Function f = env["file.exists"];
  SEXP e = f(Shield<SEXP>(wrap(con)));
  return LOGICAL(e)[0];
}
