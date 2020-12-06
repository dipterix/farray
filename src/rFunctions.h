#ifndef DIP_FARRAY_RFUN_H
#define DIP_FARRAY_RFUN_H

#include <Rcpp.h>

bool Rf2_fileExists(const std::string& file);

class RFileConn {
public:
  RFileConn();

  ~RFileConn(){
    this->close();
    this->raw_buffer = R_NilValue;
  };

  void connect(const std::string& file, const std::string& mode, bool create = true);
  void ensureLength(int64_t partition_length, int64_t size, SEXP na);
  int64_t fileBytes();
  bool exists();
  bool isValid();
  void close();
  size_t seek(int64_t where, const std::string& rw, const std::string& origin);
  void writeRaw(Rbyte* buffer, size_t len);

  std::string file;
  std::string mode;
  SEXP conn;
  Rcpp::Environment env;
  SEXP raw_buffer;

};


#endif // DIP_FARRAY_RFUN_H


