#ifndef DIP_FARRAY_FILEIO_H
#define DIP_FARRAY_FILEIO_H


#include <Rcpp.h>

SEXP r_readBin(std::string con, int64_t n, int size);

// con: file path
// buffer: char[n] (size must be at least n)
// n: number of elements to read
// size: R size of element: double is 8, int is 4...
int64_t cpp_readBin(FILE* conn, char* buffer, int64_t n,
                    int size, int64_t skip = 0, bool check_length = true);


int64_t cpp_fileLength(const std::string& con);

bool fileExists(const std::string& con);

#endif // DIP_FARRAY_FILEIO_H
