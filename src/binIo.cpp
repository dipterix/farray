#include "binIo.h"
using namespace Rcpp;

bool isLittleEndian(){
  int x = 1;
  return ( *((char*)&x) == 1 );
}

// designed for file matrix
SEXP r_readBin(std::string con, int64_t n, int size){
  Rcpp::Environment env = Rcpp::Environment::base_env();
  Rcpp::Function f = env["readBin"];
  SEXP re = f(Rcpp::Shield<SEXP>(Rcpp::wrap(con)), Rcpp::Shield<SEXP>(Rcpp::wrap("raw")), Rcpp::Shield<SEXP>(Rcpp::wrap(n * size)),
              Rcpp::Shield<SEXP>(Rcpp::wrap(1)), Rcpp::Shield<SEXP>(Rcpp::wrap(true)), Rcpp::Shield<SEXP>(Rcpp::wrap("little")));
  return re;
}

FILE* openForWriting(std::string file, std::string mode, bool create){
  FILE* conn = NULL;
  if(create){
    // test open for read
    if ((conn = fopen(file.c_str(), "rb")) == NULL){
      // file not exists, open for writing
      if((conn = fopen(file.c_str(), "wb+")) == NULL){
        return(NULL);
      }
      fclose(conn);
    }
  }
  // assume file exists
  conn = fopen(file.c_str(), "rb+");
  return(conn);
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


// bool fileExists(const std::string& con){
//   BinaryFileConn conn = BinaryFileConn(con, false);
//   return conn.isValid();
// }
