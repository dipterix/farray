#ifndef DIP_FARRAY_IO_H
#define DIP_FARRAY_IO_H

#include <cstdio>

int64_t cpp_readBin(FILE* conn, char* buffer, int64_t n, int size, int64_t skip = 0, bool check_length = true);

#endif // DIP_FARRAY_IO_H
