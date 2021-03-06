#ifndef DIP_FARRAY_FILEIO_H
#define DIP_FARRAY_FILEIO_H


#include <cstdio>
#include <Rcpp.h>
#include "common.h"


SEXP r_readBin(std::string con, int64_t n, int size);

// [[Rcpp::export]]
bool isLittleEndian();

FILE* openForWriting(std::string file, std::string mode = "rb+", bool create = true);

class BinaryFileConn {
public:
  BinaryFileConn() {
    this->file = "";
    this->conn = NULL;
    this->mode = "rb";
  }
  BinaryFileConn(const std::string& file, bool write = false, bool create = true) {
    this->file = file;
    try {
      if(write) {
        this->mode = "rb+";
        this->conn = openForWriting(file, "rb+", create);
      } else {
        this->mode = "rb";
        this->conn = fopen(file.c_str(), "rb");
      }
    } catch(...){
      this->mode = "rb";
      this->conn = NULL;
    }
  };

  bool connect(const std::string& file, bool write = false, bool create = true) {
    this->close();
    try {
      this->file = file;
      if(write) {
        this->mode = "rb+";
        this->conn = openForWriting(file, "rb+", create);
      } else {
        this->mode = "rb";
        this->conn = fopen(file.c_str(), "rb");
      }
    } catch(...){
      this->conn = NULL;
    }
    return ( this->isValid() );
  };

  ~BinaryFileConn(){
    this->close();
  };

  bool isValid() {
    return(this->conn != NULL);
  };

  bool close() {
    if(this->conn != NULL) {
      try{
        fclose(this->conn);
      } catch(...){
        return(false);
      }
      this->conn = NULL;
    }
    return(true);
  };

  std::string file;
  FILE* conn;
  std::string mode;
};


template <typename T>
void revEndian(const T* ptr, size_t n) {
  size_t size = sizeof(T);
  size_t half = 0;
  unsigned char *buffer = (unsigned char*) ptr;
  unsigned char a;
  for (size_t i=0; i<n; i++) {
    for(half = 0; half * 2 <= size; half++){
      a = buffer[size * i +  half];
      buffer[size * i +  half] = buffer[size * i + size - 1 - half];
      buffer[size * i + size - 1 - half] = a;
    }
  }
};

template <typename T>
size_t fwriteLEndian(const T *ptr, size_t n, FILE *stream) {
  size_t size = sizeof(T);
  if ( isLittleEndian() ) {
    /* Little endian machine, use fwrite directly */
    return fwrite(ptr, size, n, stream);
  } else {
    /* Big endian machine, pre-process first */
    size_t half = 0;
    size_t count = 0;
    T tmp[1];
    unsigned char *buffer = (unsigned char*) tmp;
    unsigned char a;
    for (size_t i=0; i<n; i++) {
      *tmp = *(ptr + i);
      for(half = 0; half * 2 <= size; half++){
        a = buffer[half];
        buffer[half] = buffer[size - 1 - half];
        buffer[size - 1 - half] = a;
      }
      count += fwrite(tmp, size, 1, stream);
    }
    return count;
    // revEndian<T>(ptr, n);
    // return fwrite(ptr, size, n, stream);
  }
}


// MUST open in "rb+" mode!!!
template <typename T>
int64_t cpp_writeBin(FILE* conn, T* data, size_t dLen, T na, int64_t skip = 0) {
  // FILE* conn, char* buffer, int64_t n, int size, int64_t skip, bool check_length

  // index0 starts from 0
  if(!dLen) { return (0); }
  int64_t element_size = sizeof(T);

  int64_t flen_byte = 0;
  int64_t skip_byte = skip * element_size;
  const int64_t buffer_size = FARRAY_BUFFERSIZE / element_size;
  fseek(conn, 0, SEEK_END);
  flen_byte = ftell(conn);
  T na_copy;

  // skip_byte
  T* ptr_data = data;
  size_t el_written = 0;

  if(skip_byte > flen_byte){
    int64_t na_bytes = skip_byte - flen_byte;
    fseek(conn, 0, SEEK_END);
    for(; na_bytes > 0; na_bytes -= element_size) {
      na_copy = na;
      el_written += fwriteLEndian<T>(&na_copy, 1, conn);
    }
    fflush(conn);
    el_written = 0;
  }

  fseek(conn, skip_byte, SEEK_SET);

  int64_t rest = dLen;
  while(rest > 0){
    if( rest >= buffer_size ){
      fwriteLEndian<T>(&(*ptr_data), buffer_size, conn);
      rest -= buffer_size;
      ptr_data += buffer_size;
    } else {
      fwriteLEndian<T>(&(*ptr_data), rest, conn);
      rest -= rest;
    }
    fflush(conn);
  }

  return dLen;
}


// con: file path
// buffer: char[n] (size must be at least n)
// n: number of elements to read
// size: R size of element: double is 8, int is 4...
// int64_t cpp_readBin(FILE* conn, char* buffer, int64_t n,
//                     int size, int64_t skip = 0, bool check_length = true);

template <typename T>
int64_t cpp_readBin(FILE* conn, T* buffer, int64_t n, int64_t skip = 0, bool check_length = true){
  // char* buffer = new char[n * size];
  // std::ifstream input( con, std::ios::binary );
  int size = sizeof(T);
  int64_t fsize = 1;
  int64_t n_byte = n * size;
  int64_t skip_byte = skip * size;
  char* ptr = (char*) buffer;
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
  // reverse endianess
  if(!isLittleEndian()){
    revEndian(buffer, n);
  }
  return n_byte;
};

int64_t cpp_fileLength(const std::string& con);

template <typename T>
int64_t cpp_fillPartition(std::string& file, int64_t n, T na, bool overwrite = false) {
  BinaryFileConn conn = BinaryFileConn(file, true);
  if(!conn.isValid()){
    stop("C++: `cpp_fillPartition`: Cannot open file for writing");
  }
  // check file length
  int64_t skip = 0;
  int64_t restN = n;
  int size = sizeof(T);
  if( !overwrite ){
    fseek(conn.conn, 0, SEEK_END);
    skip = ftell(conn.conn) / size;
    restN = n - skip;
    fseek(conn.conn, 0, SEEK_END);
  } else {
    fseek(conn.conn, 0, SEEK_SET);
  }
  int64_t bufferSize = FARRAY_BUFFERSIZE;

  if(bufferSize > restN){ bufferSize = restN; }
  if(n <= 0 || bufferSize <= 0){
    conn.close();
    return (skip);
  }
  std::vector<T> nas = std::vector<T>(bufferSize, na);

  while(restN > 0) {
    if( bufferSize > restN ){
      bufferSize = restN;
    }
    cpp_writeBin<T>(conn.conn, &(nas[0]), bufferSize, na, skip);
    skip += bufferSize;
    restN -= bufferSize;
  }
  conn.close();
  return (n);
}

bool fileExists(const std::string& con);


#endif // DIP_FARRAY_FILEIO_H
