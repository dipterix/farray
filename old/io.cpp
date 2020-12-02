#include "io.h"

int64_t cpp_readBin(FILE* conn, char* buffer, int64_t n,
                    int size, int64_t skip, bool check_length){
  int64_t fsize = 0;
  int64_t n_byte = n * size;
  try{
    if(check_length){
      fseek(conn, 0, SEEK_END);
      fsize = ::ftell(conn);
      if(fsize < size){
        n_byte = 0;
      } else {
        if(fsize < n_byte){
          n_byte = fsize;
        }
        fseek(conn, skip * size, SEEK_SET);
        fsize = std::fread(buffer, 1, n_byte, conn);
      }
    } else {
      fseek(conn, skip * size, SEEK_SET);
      fsize = std::fread(buffer, 1, n_byte, conn);
    }
  } catch (...) {
    n_byte = 0;
  }
  return n_byte;
}
