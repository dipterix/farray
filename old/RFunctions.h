#ifndef DIP_FARRAY_RFUN_H
#define DIP_FARRAY_RFUN_H

#include <string>

std::string Rf2_normalizePath(const std::string& path, const bool& mustWork = true);

std::vector<int64_t> Rf2_uniqueIndex(const std::vector<int64_t>& x);

#endif  // DIP_FARRAY_RFUN_H
