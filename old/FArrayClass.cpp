// #include "Common.h"
// #include "RFunctions.h"
// #include "ArrayIndex.h"
// #include "io.h"
// #include "FArrayClass.hpp"
// using namespace Rcpp;
// // --------------------------- Constructor ------------------------------
//
// template <typename T>
// FArray<T>::FArray(
//   const std::string& rootPath,
//   const std::vector<int64_t>& dim,
//   const std::vector<int64_t>& bDim,
//   SEXPTYPE dType,
//   T na
// ): dim(dim), bDim(bDim), dType(dType){
//   this->rootPath = Rf2_normalizePath(rootPath);
//
//   // calculate iDim and fDim
//   int ndims = dim.size();
//   int bDimSize = bDim.size();
//   int fDimSize = ndims - bDimSize;
//   if(fDimSize <= 0){
//     Rcpp::stop("C++: `FArray-Class`: total dimension size should be greater than block dimension");
//   }
//   this->iDim = std::vector<int64_t>(bDimSize);
//   this->fDim = std::vector<int64_t>(fDimSize);
//   for( int dd = 0 ; dd < bDimSize; dd++ ){
//     int rem = dim[dd] % bDim[dd];
//     if(rem == 0){
//       this->iDim[dd] = dim[dd];
//     } else {
//       this->iDim[dd] = dim[dd] - rem + bDim[dd];
//     }
//   }
//   for(int dd = 0; dd < fDimSize; dd++){
//     this->fDim[dd] = dim[dd + bDimSize];
//   }
//
//
//   this->cacheKey = "";
//   this->cachedIndex = std::vector<std::vector<int64_t>>();
//   this->na = na;
// }
//
// // --------------------------- member functions ------------------------------
//
// // Always store cachedIndex and cacheKey
// // Except: if cacheKey != "" and this.cacheKey == cacheKey, then return cachedIndex
// template <class T>
// std::vector<std::vector<int64_t>> FArray<T>::parseIndex(const SEXP listOrEnv, std::string cacheKey) {
//   if(cacheKey.compare("") || !cacheKey.compare(this->cacheKey) || this->cachedIndex.size() != 3){
//     this->cachedIndex = listOrEnv2Idx2(listOrEnv, this->dim, this->bDim);
//   }
//   return this->cachedIndex;
// }
//
// template <class T>
// void FArray<T>::removeCachedIndex(){
//   this->cacheKey = "";
//   this->cachedIndex = std::vector<std::vector<int64_t>>();
// }
//
// // subset array using cachedIndex
// // if listOrEnvOrNil is R_NilValue and cachedIndex exists (length == ndim), use cachedIndex
// // if listOrEnvOrNil is not R_NilValue, call parseIndex and use cachedIndex
// template <class T>
// void FArray<T>::subsetArray(SEXP results, SEXP listOrEnvOrNil){
//   // No check! (no error handling, make sure cachedIndex exists or listOrEnvOrNil is not null)
//   if( listOrEnvOrNil != R_NilValue ){
//     this->parseIndex(listOrEnvOrNil, "");
//   }
//
//   // check if SEXP is consistent
//   if(TYPEOF(results) != this->dType){
//     Rcpp::stop("C++ `FArray-Class`: SEXPTYPE does not match");
//   }
//   // check if cachedIndex is valid
//   if(this->cachedIndex.size() != 4){
//     Rcpp::stop("C++ `FArray-Class`: cachedIndex is not a vector(4)");
//   }
//   // Index:
//   // 1: sub index within block
//   // 2: block index
//   // 3. file index
//   // 4. target dimension
//
//   // get index
//   std::vector<int64_t> subBlockIdx   = this->cachedIndex[0];
//   std::vector<int64_t> blockIdx      = this->cachedIndex[1];
//   std::vector<int64_t> fileIdx       = this->cachedIndex[2];
//   std::vector<int64_t> tDim          = this->cachedIndex[3];
//   // check if results length is valid
//   int64_t tLen = Rf_xlength(results);
//   int64_t expected_len = std::accumulate(tDim.begin(), tDim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
//   if(tLen != expected_len){
//     Rcpp::stop("C++: `FArray-Class`: result length does not match.");
//   }
//   int64_t tPLen = blockIdx.size(); // target partition length
//
//   // Find unique block sets
//   std::vector<int64_t> uniqueBlockSet = uniqueIndexSet(blockIdx);
//   // buffer size
//   int64_t bLen = std::accumulate(this->bDim.begin(), this->bDim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
//   // int64_t iLen = std::accumulate(this->iDim.begin(), this->iDim.end(), INTEGER64_ONE, std::multiplies<int64_t>());
//   int64_t bufferSize = bLen;
//   const T* ptr_res_begin = (T*)(RAW(results));
//
//   // reserved for OPENMP
//   {
//     // Schedule buffers
//     T buffer[bufferSize]; // private
//     T* ptr_buffer = buffer;
//     T* ptr_res = ptr_res_begin;
//     int64_t ii;
//     std::vector<int64_t>::iterator ptr_subBlockIdx;
//     std::vector<int64_t>::iterator ptr_blockIdx;
//     int64_t fid;
//     std::string fname;
//     bool fileio_error;
//     FILE* input;
//
//     // Read data to results, openmp for
//     for(int64_t fii = 0; fii < fileIdx.size(); fii++){
//       for(int64_t bii = 0; bii < uniqueBlockSet.size(); bii++){
//
//         fid = fileIdx[fii];
//         fname = this->rootPath + "/" + std::to_string(fid + 1) + ".bmat";
//         int64_t currentBlockIdx = uniqueBlockSet[bii];
//         fileio_error = false;
//
//         ptr_res = ptr_res_begin + tPLen * fii;
//
//         try{
//           input = fopen( fname.c_str(), "rb" );
//         } catch(...){
//           fileio_error = true;
//         }
//
//         if(!fileio_error){
//           try{
//             cpp_readBin(input, (char*)(buffer), bLen, sizeof(T), currentBlockIdx * bufferSize, false);
//           } catch (...) {
//             fileio_error = true;
//           }
//         }
//         if(fileio_error){
//           for(ptr_buffer = buffer, ii = 0; ii < bufferSize; ii++){
//             *ptr_buffer++ = this->na;
//           }
//         }
//         // fill in the values
//
//
//         for(ptr_subBlockIdx = subBlockIdx.begin(), ptr_blockIdx = blockIdx.begin();
//             ptr_subBlockIdx != subBlockIdx.end(); ptr_subBlockIdx++, ptr_blockIdx++, ptr_res++) {
//           if(*ptr_blockIdx == currentBlockIdx){
//             // element value *(buffer + *ptr_subBlockIdx)
//             *ptr_res = *(buffer + *ptr_subBlockIdx);
//           }
//         }
//
//
//         if(input != NULL){
//           try{
//             fclose(input);
//           } catch(...){}
//         }
//
//       }
//     }
//
//   }
//
//
//
//
//
//   // Slice!
//   std::vector<T> slice = std::vector<T>();
//
//   // clean up
//   if( (this->cacheKey).compare("") ){
//     this->removeCachedIndex();
//   }
// }
//
//
