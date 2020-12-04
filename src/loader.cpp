#include <cstdio>
#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
#include "openMPInterface.h"
#include "binIo.h"
using namespace Rcpp;


template <SEXPTYPE RTYPE, typename T>
SEXP subsetFMtemplate(const std::string& rootPath, const std::vector<int64_t>& dim,
                      const ParsedIndex* subparsed){
  int nThread = getFArrayThread();
  if(nThread <= 1){ nThread = 1; }

  int subset_mode = subparsed->subset_mode;
  const std::vector<int64_t> target_dimension = subparsed->target_dimension;
  const int64_t expected_length = subparsed->expected_length;
  const std::vector<bool> negative_subscript = subparsed->negative_subscript;
  std::vector<std::pair<std::vector<int64_t>, bool>> location_indices = subparsed->location_indices;

  StringVector cnames;
  const bool is_complex = RTYPE == CPLXSXP;
  if(is_complex){
    cnames = StringVector::create("V1R", "V1I");
  } else {
    cnames = StringVector::create("V1");
  }

  // create results
  Vector<RTYPE> res = static_cast<Vector<RTYPE>>(no_init(expected_length));
  int element_size = sizeof(T);

  // for blocked runs (if needed)
  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t expect_nrows = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());

  int64_t chunk_start, chunk_end;
  int64_t reader_start, reader_end;
  int64_t nfiles = *(dim.end() - 1);
  std::string partition_path;

  // Vector<RTYPE> buffer = Vector<RTYPE>(0);
  // auto ptr_buffer = buffer.begin();
  auto na_value = Vector<RTYPE>::get_na();

  if( subset_mode == LASUBMOD_SINGLE && std::get<1>(location_indices[0]) ){
    subset_mode = LASUBMOD_NOIDX;
  }


  if(subset_mode == LASUBMOD_NOIDX){
    // case: subset_mode == 2, x[]

    tok("S subsetFMtemplate - LASUBMOD_NOIDX");

    // create buffer
    // std::vector<char> buffer(buffer_size);

    // n buffers per partition
    // int64_t nblocks = expect_nrows / BLOCKSIZE;
    // nblocks = (nblocks * BLOCKSIZE) > expect_nrows? nblocks - 1: nblocks;

    // last time to exit before omp loop
    R_CheckUserInterrupt();

#pragma omp parallel num_threads(nThread) private(chunk_end, chunk_start, reader_start, reader_end)
{
  // schedule private variables
  auto ptr_res_private = res.begin();
  auto ptr_alt_private = res.begin();
  std::string partition_path_private;
  bool bugged = false;
  BinaryFileConn fcon = BinaryFileConn(); // = BinaryFileConn(file, true);
  // FILE* input = NULL;

#pragma omp for schedule(static, 1) nowait
{
    for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++ ){

      partition_path_private = rootPath + std::to_string(file_ii) + ".bmat";
      // R_CheckUserInterrupt();
      ptr_res_private = res.begin() + (block_size) * (file_ii - 1);
      // try to directly open the file
      // input = NULL;
      bugged = false;

      // It's possible the file cannot be opened
      fcon.connect( partition_path_private, false );

      if(fcon.isValid()) {
        try {
          if(cpp_fileLength(partition_path_private) == (expect_nrows * element_size)) {
            // cpp_readBin(input, (char*)(ptr_res_private), expect_nrows, element_size, 0, false);
            cpp_readBin<T>(fcon.conn, ptr_res_private, expect_nrows, 0, false);
          } else {
            bugged = true;
          }
        } catch (...) {
          // cannot open the partition
          bugged = true;
        }
        fcon.close();
      } else {
        // file missing, fill with NA
        bugged = true;
      }

      if( bugged ){
        // this file is invalid, fill with na
        ptr_alt_private = ptr_res_private + block_size;
        while( ptr_alt_private != ptr_res_private ){
          *ptr_res_private++ = na_value;
        }
      }

    }
} // end omp for
} // end omp

    res.attr("dim") = Shield<SEXP>(wrap(target_dimension));

    tok("E subsetFMtemplate - LASUBMOD_NOIDX");

    // } else if(subset_mode == LASUBMOD_SINGLE){
    //
    //     tok("S subsetFMtemplate - LASUBMOD_SINGLE");
    //     // case: subset_mode == 1, x[i], and i can't be R missing value
    //     std::vector<int64_t> indices = std::get<0>(location_indices[0]);
    //     std::vector<int64_t>::iterator ptr_indices = indices.begin();
    //     bool is_negative = negative_subscript[0];
    //
    //     // idx from chunk_start to expect_nrows-1
    //     chunk_start = 0;
    //     chunk_end = expect_nrows;
    //     std::vector<bool> sel = std::vector<bool>(indices.size());
    //     std::vector<bool>::iterator ptr_sel = sel.begin();
    //     int64_t bump_start = 0;
    //     bool enable_bump = true;
    //     // TODO: check negative cases
    //     if(is_negative){
    //
    //       // There is simply no point to do so, data is so large and I'll defer the implementation
    //       stop("Negative subscript has not been implemented yet.");
    //       // // No NAs in this case, just iterate through all
    //       // // indices is in decending order, start from the end
    //       // for(StringVector::iterator ptr_file = files.begin(); ptr_file != files.end();
    //       //     ptr_file++, chunk_start += expect_nrows, chunk_end += expect_nrows ) {
    //       //
    //       // }
    //
    //     } else {
    //       // initialize with NA
    //       ptr_res = res.begin();
    //       ptr_alt = ptr_res;
    //       ptr_res += expected_length;
    //       for(;ptr_alt != ptr_res; ptr_alt++){
    //         *ptr_alt = na_value;
    //       }
    //       ptr_res = res.begin();
    //       std::vector<int64_t> sub_idx(0);
    //       for(int64_t file_ii = 1; file_ii <= nfiles; file_ii++, chunk_start += expect_nrows, chunk_end += expect_nrows ){
    //         partition_path = rootPath + std::to_string(file_ii) + ".fst";
    //         R_CheckUserInterrupt();
    //
    //         int64_t sub_count = 0;
    //         for(ptr_sel = sel.begin(), ptr_indices = indices.begin();
    //             ptr_sel != sel.end(); ptr_sel++, ptr_indices++ ){
    //           // na is negative so falls in this range
    //           if(*ptr_indices <= chunk_start || *ptr_indices > chunk_end || *ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64){
    //             *ptr_sel = false;
    //           } else {
    //             *ptr_sel = true;
    //             sub_count++;
    //           }
    //         }
    //
    //         // if this chunk is not used, just skip;
    //         if( sub_count == 0 ){ continue; }
    //
    //         // get sub index for this chunk, note no NA is included
    //         sub_idx.resize(sub_count);
    //         std::vector<int64_t>::iterator ptr_sub_idx = sub_idx.begin();
    //         reader_start = -1;
    //         reader_end = -1;
    //
    //         for(ptr_sel = sel.begin(), ptr_indices = indices.begin();
    //             ptr_sub_idx != sub_idx.end() && ptr_indices != indices.end() && ptr_sel != sel.end();
    //             ptr_indices++, ptr_sel++){
    //           if(*ptr_sel){
    //             *ptr_sub_idx = *ptr_indices;
    //             if(reader_start > *ptr_indices || reader_start == -1){
    //               reader_start = (int)(*ptr_indices);
    //             }
    //             if(reader_end < *ptr_indices){
    //               reader_end = (int)(*ptr_indices);
    //             }
    //             // Rcout << *ptr_sub_idx << " ";
    //             ptr_sub_idx++;
    //           }
    //         }
    //
    //         reader_start -= chunk_start;
    //         reader_end -= chunk_start;
    //         // Rcout << "\n" << std::to_string(reader_start) << " " << std::to_string(reader_end) << "\n";
    //
    //         // read to buffer
    //         if( !checkFstMeta(partition_path, expect_nrows, cnames) ){
    //           // this file is invalid, fill with na, but they have been set to NAs
    //           continue;
    //         }
    //         // Read from fst file, abuse name "meta" a little bit
    //         tmp = fstRetrieve(partition_path, Shield<SEXP>(wrap(cnames)),
    //                           Shield<SEXP>(wrap(reader_start)), Shield<SEXP>(wrap(reader_end)));
    //         tmp = VECTOR_ELT(tmp, 2);
    //
    //         if(is_complex){
    //           // try to reuse buffer, but if not reusable, create new one
    //           if(buffer.size() != reader_end - reader_start + 1){
    //             buffer = static_cast<Vector<RTYPE>>(no_init(reader_end - reader_start + 1));
    //           }
    //           setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 0)), true);
    //           setReIm(static_cast<ComplexVector>(buffer), as<NumericVector>(VECTOR_ELT(tmp, 1)), false);
    //         } else {
    //
    //           SEXP tmp_var = VECTOR_ELT(tmp, 0);
    //           if(TYPEOF(tmp_var) != RTYPE){
    //             tmp_var = PROTECT(Rf_coerceVector(tmp_var, RTYPE));
    //             buffer = as<Vector<RTYPE>>(tmp_var);
    //             UNPROTECT(1);
    //           } else {
    //             buffer = as<Vector<RTYPE>>(tmp_var);
    //           }
    //
    //         }
    //
    //         ptr_res = res.begin();
    //         ptr_alt = ptr_res + expected_length;
    //         // Skip first bump_start elements as they are all set
    //         ptr_res = ptr_res + bump_start;
    //         ptr_sub_idx = sub_idx.begin();
    //         ptr_sel = sel.begin() + bump_start;
    //         ptr_indices = indices.begin()+ bump_start;
    //         enable_bump = false; // FIXME?
    //
    //         while(ptr_sel != sel.end() && ptr_sub_idx != sub_idx.end() && ptr_res != ptr_alt){
    //
    //           if(*ptr_sel){
    //             *ptr_res = *(buffer.begin() + (*ptr_sub_idx++ - chunk_start - reader_start));
    //             *ptr_indices = NA_INTEGER64;
    //             if( enable_bump ){
    //               bump_start++;
    //             }
    //           } else if(enable_bump && (*ptr_indices == NA_REAL || *ptr_indices == NA_INTEGER64)){
    //             bump_start++;
    //           } else {
    //             enable_bump = false;
    //           }
    //           // Rcout << bump_start << " ";
    //           ptr_sel++;
    //           ptr_res++;
    //           ptr_indices++;
    //         }
    //
    //       }
    //
    //     }
    //     tok("E subsetFMtemplate - LASUBMOD_SINGLE");
    //
  } else if (subset_mode == LASUBMOD_MULTI) {
    // case: subset_mode == 0, x[i,j,k,l,...], and ijk might be R missing value
    tok("S subsetFMtemplate - LASUBMOD_MULTI");
    // get parsed schedules
    // dim = [block dim, schedule dim, partition counts]
    // [5,16] => block_ndims=0, [5] x [1,1] x [16]
    // [3000,7,3] if block_ndims=0 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=1 => [3000] x [7,1] x [3]
    // [3000,7,3] if block_ndims=2 => [3000,7] x [1,1] x [3]
    // [100,100,100,100,1] block_ndims=2 => [100,100] x [100,100,1] x [1]

    ScheduledIndex* schedule = subparsed->schedule;

    // partition level
    // int64_t partition_counts = schedule["partition_counts"];               // the last dimension - n files to iterate through
    std::vector<int64_t> partition_index = schedule->partition_index;    // detailed indexes   - always exists

    // schedule level
    // int64_t schedule_counts_per_part = schedule["schedule_counts_per_part"]; // for each partition, number of blocks to run
    std::vector<int64_t> schedule_index = schedule->schedule_index;      // indices to schedule run blocks
    std::vector<int64_t> schedule_dimension = schedule->schedule_dimension; // [schedule dim, partition counts]

    // block level
    int64_t block_ndims = schedule->block_ndims;                    // length(block dim)
    std::vector<int64_t> block_dimension = schedule->block_dimension; // [block dim], full version
    std::vector<int64_t> block_prod_dim = schedule->block_prod_dim; // prod([1, block dim]), used to locate indices when block is too large to index
    std::vector<int64_t> block_schedule = schedule->block_schedule; // given a flattened block (full version), which indices to subset?
    int64_t block_schedule_start = schedule->block_schedule_start;
    int64_t block_schedule_end = schedule->block_schedule_end;      // min, max of block_schedule

    int64_t block_length = schedule->block_length;                  // # elements in a block (full version) = prod(block_dimension)
    int64_t block_expected_length = schedule->block_expected_length;// # elements in a block (subset version) = length(block_schedule)

    bool block_indexed = schedule->block_indexed;                   // whether block_schedule can be trusted
    std::vector<std::pair<std::vector<int64_t>, bool>> block_location = schedule->block_location;           // subset of locational indices of blocks

    // DEBUG, TODO: remove
    // block_indexed = false;



    // create buffers
    int64_t total_schedules = schedule_index.size();
    // int omp_chunk = (int) partition_index.size();
    // if(total_schedules - omp_chunk < 0 ){
    //   omp_chunk = (int) total_schedules;
    // }
    // if(omp_chunk < 1){
    //   omp_chunk = 1;
    // }

    int64_t buffer_xlen;
    if(block_indexed) {
      buffer_xlen = block_schedule_end - block_schedule_start + 1;
      if(buffer_xlen > BLOCKBUFFER){
        buffer_xlen = BLOCKBUFFER;
      }
    } else {
      buffer_xlen = BLOCKBUFFER;
    }

#pragma omp parallel num_threads(nThread) private(chunk_end, chunk_start, reader_start, reader_end)
{
  // private variables
  // schedule private variables
  auto ptr_res_private = res.begin();
  auto ptr_alt_private = res.begin();
  std::string partition_path_private;
  bool bugged = false;
  BinaryFileConn fcon = BinaryFileConn();
  // FILE* input;
  int64_t file_len;
  int64_t lidx;
  T buffer[buffer_xlen];
  T* ptr_buffer = buffer;
  int64_t block_number;
  std::vector<int64_t>::iterator ptr_block_schedule;
  int64_t buffer_pos;

  // for non-indexed array
  int64_t mod, rest, sub_index, subblock_dim_ii, tmp;

#pragma omp for collapse(2) schedule(static, 1) nowait
{
  for(int64_t li = 0; li < partition_index.size(); li++){
    for(int64_t schedule_ii = 0; schedule_ii < total_schedules; schedule_ii++){

      lidx = partition_index[li];

      // block_size = block_expected_length * schedule_counts_per_part
      // only in charge of ptr_res_private ~ ptr_res_private + block_expected_length
      ptr_res_private = res.begin() + block_size * li + schedule_ii * block_expected_length;

      // Fill in NA if *ptr_last_idx is NA_REAL
      if(lidx == NA_INTEGER64 || lidx == NA_REAL){
        // name is messed up here,
        // block_size = block_expected_length * schedule_counts_per_part
        bugged = true;
        // continue;
      } else {

        // Check is file valid?
        partition_path_private = rootPath + std::to_string(lidx) + ".bmat";
        // input = NULL;
        bugged = false;

        // It's possible the file cannot be opened
        fcon.connect( partition_path_private, false );

        if(fcon.isValid()) {
          try {
            file_len = cpp_fileLength(partition_path_private);
            if(file_len == (expect_nrows * element_size)) {
              // This is a valid connection, read data here!!!!!!!!

              // read elements as this will put the file to warm start
              //
              chunk_start = 0;
              chunk_end = block_length;

              ptr_buffer = buffer;
              // re-calculate index position for pointers (they are private now)
              // ptr_res2 ~ (ptr_res2 + block_expected_length) are to write
              // ptr_res_private = res.begin() + (block_size * li + schedule_ii * block_expected_length);

              block_number = schedule_index[schedule_ii];

              if(block_number == NA_INTEGER64 || !(block_schedule_start > 0 && block_schedule_start <= block_schedule_end)){
                // fill NAs
                ptr_alt_private = ptr_res_private + block_expected_length; // block length (subset version)
                for(;ptr_res_private != ptr_alt_private; ptr_res_private++){
                  *ptr_res_private = na_value;
                }
                // continue;
              } else {
                // locate where the rows are in the file
                chunk_end = block_length * block_number;
                chunk_start = chunk_end - block_length;
                // int64_t subblock_min = (min subblock_idx), subblock_max = max subblock_idx;
                reader_start = (chunk_start + block_schedule_start);
                reader_end = (chunk_start + block_schedule_end);

                //#pragma omp critical // ?
                if( block_indexed ){

                  // load buffer
                  if(file_len < (reader_start-1 + buffer_xlen) * element_size){
                    // cpp_readBin(input, (char*) buffer, buffer_xlen, element_size, reader_start-1, true);
                    cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1, true);
                  } else {
                    // cpp_readBin(input, (char*) buffer, buffer_xlen, element_size, reader_start-1, false);
                    cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1, false);
                  }
                  buffer_pos = 0;

                  // don't calculate index on the fly.
                  for(ptr_block_schedule = block_schedule.begin(); ptr_block_schedule != block_schedule.end(); ptr_block_schedule++)
                  {
                    if(*ptr_block_schedule < block_schedule_start || *ptr_block_schedule == NA_REAL ||
                       *ptr_block_schedule == NA_INTEGER64){
                       *ptr_res_private++ = na_value;
                    } else {
                      sub_index = *ptr_block_schedule - block_schedule_start - buffer_pos;
                      if(sub_index < 0 || sub_index >= buffer_xlen){
                        // need to load new data
                        buffer_pos += sub_index;
                        if(file_len < (reader_start-1 + buffer_xlen + buffer_pos) * element_size){
                          // cpp_readBin(input, (char*) buffer, buffer_xlen, element_size, reader_start-1 + buffer_pos, true);
                          cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1+buffer_pos, true);
                        } else {
                          // cpp_readBin(input, (char*) buffer, buffer_xlen, element_size, reader_start-1 + buffer_pos, false);
                          cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1+buffer_pos, false);
                        }
                      }
                      *ptr_res_private++ = *(buffer + sub_index);
                    }
                  }
                } else {
                  // load buffer
                  if(file_len < (reader_start-1 + buffer_xlen) * element_size){
                    // cpp_readBin(fcon.conn, (char*) buffer, buffer_xlen, element_size, reader_start-1, true);
                    cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1, true);
                  } else {
                    // cpp_readBin(fcon.conn, (char*) buffer, buffer_xlen, element_size, reader_start-1, false);
                    cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1, false);
                  }
                  buffer_pos = 0;

                  // non-indexed (usually memory too big for index), index on the fly

                  for(int64_t ii = 0; ii < block_expected_length; ii++ ){
                    rest = ii;
                    sub_index = 0;
                    for(int64_t di = 0; di < block_ndims; di++ ){

                      // block_dimension = schedule["block_dimension"]; // [block dim], full version
                      // std::vector<int64_t> block_prod_dim = schedule["block_prod_dim"]; // prod([1, block dim]), used to locate indices when block is too large to index
                      // std::vector<int64_t> block_schedule = schedule["block_schedule"]; // given a flattened block (full version), which indices to subset?
                      // int64_t block_schedule_start = schedule["block_schedule_start"];
                      // int64_t block_schedule_end = schedule["block_schedule_end"];      // min, max of block_schedule
                      //
                      // int64_t block_length = schedule["block_length"];                  // # elements in a block (full version) = prod(block_dimension)
                      // int64_t block_expected_length = schedule["block_expected_length"];// # elements in a block (subset version) = length(block_schedule)
                      //
                      // bool block_indexed = schedule["block_indexed"];                   // whether schedule_index can be trusted
                      // const List block_location

                      if(sub_index != NA_INTEGER64 && sub_index != NA_REAL){
                        subblock_dim_ii = *(target_dimension.begin() + di);
                        mod = rest % subblock_dim_ii;
                        rest = (rest - mod) / subblock_dim_ii;


                        // get di^th margin element mod
                        // partition_subblocklocs[di][mod]

                        // check this lication is empty
                        if( std::get<1>(block_location[di]) ){
                          // index[di], location_ii is missing
                          tmp = mod + 1;
                        } else {
                          // index[di]
                          tmp = std::get<0>(block_location[di])[mod];
                        }
                        // print(wrap(location_ii));
                        // Rcout << di << " " << mod << " " << tmp << " ";
                        // is tmp is < 1, that mean it's invalid may be remove the other one
                        if(tmp < 1 || tmp == NA_REAL ){
                          sub_index = NA_INTEGER64;
                        } else if (tmp != NA_INTEGER64){
                          // location_ii starts from 1 but we need it to starting from 0
                          sub_index += *(block_prod_dim.begin() + di) * (tmp - 1);
                        }
                      }


                    }
                    // Rcout << block_schedule_start << " " << sub_index<< "\n";

                    if( sub_index == NA_INTEGER64 ) {
                      *ptr_res_private++ = na_value;
                    } else {
                      sub_index = sub_index + 1 - block_schedule_start - buffer_pos;

                      if(sub_index < 0 || sub_index >= buffer_xlen){
                        // need to load new data
                        buffer_pos += sub_index;
                        if(file_len < (reader_start-1 + buffer_xlen + buffer_pos) * element_size){
                          // cpp_readBin(fcon.conn, (char*) buffer, buffer_xlen, element_size, reader_start-1 + buffer_pos, true);
                          cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1+buffer_pos, true);
                        } else {
                          // cpp_readBin(fcon.conn, (char*) buffer, buffer_xlen, element_size, reader_start-1 + buffer_pos, false);
                          cpp_readBin<T>(fcon.conn, buffer, buffer_xlen, reader_start-1+buffer_pos, false);
                        }
                      }

                      *ptr_res_private++ = *(buffer + sub_index);
                    }
                    // Rcout << *(ptr_res + ii) << "\n";
                  }

                }
              }

              // Finished reading !!!!!!!!!
              bugged = false;
            } else {
              bugged = true;
            }
          } catch (...) {
            // cannot open the partition
            bugged = true;
          }
          fcon.close();
        } else {
          // file missing, fill with NA
          bugged = true;
        }
      }

      if( bugged ){
        ptr_res_private = res.begin() + block_size * li + schedule_ii * block_expected_length;
        // this file is invalid, fill with na
        ptr_alt_private = ptr_res_private + block_expected_length;
        while( ptr_alt_private != ptr_res_private ){
          *ptr_res_private++ = na_value;
        }
      }
    }
  }

} // end omp parallel num_threads(nThread)

// if(fileio_error){
//   warning("Error while reading partition file(s)");
// }
// if(input != NULL){
//   fclose( input );
// }

// make sure pointer is correct
// ptr_res += block_size;

    }

    res.attr("dim") = wrap(target_dimension);

    tok("E subsetFMtemplate - LASUBMOD_MULTI");

  } else {
    stop("Unknown subset method");
  }


  return res;
}


SEXP subsetFMBare(const std::string& rootPath, const ParsedIndex* parsed,
                  const std::vector<int64_t>& dim, const SEXPTYPE& dtype) {
  tok("S subsetFSTBare");
  SEXP res = R_NilValue;
  const std::string rootPath_alt = as_dirpath(rootPath);
  std::vector<int64_t> dim_alt = as<std::vector<int64_t>>(wrap(dim));

  switch(dtype){
  case REALSXP:
    res = subsetFMtemplate<REALSXP, double>(rootPath_alt, dim_alt, parsed);
    break;
  case INTSXP:
    res = subsetFMtemplate<INTSXP, int>(rootPath_alt, dim_alt, parsed);
    break;
  default:
    stop("Unknown data type for FileArray: only numeric, integer are supported - provided SEXPTYPE: " + std::to_string(dtype));
  }
  tok("E subsetFSTBare");
  return res;
}

// [[Rcpp::export]]
SEXP subsetFM(const std::string& rootPath, SEXP listOrEnv, const std::vector<int64_t>& dim, SEXPTYPE dtype, SEXP reshape, bool drop){
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  tok("S subsetFM");
  ParsedIndex* tp = parseAndScheduleBlocks(listOrEnv, dim);

  SEXP res = subsetFMBare(rootPath, tp, dim, dtype);

  delete tp;

  reshapeOrDrop(res, reshape, drop);
  tok("E subsetFM");
  return res;
}


/*** R
# devtools::load_all(); f <- normalizePath('~/Desktop/filearray_data/'); subsetFM(f, list(1,1,1,1), c(287,200,601,1), 14L, NULL, TRUE)

devtools::load_all();
filearray <- filearray('~/Desktop/filearray_data/', dim = dim(flazy))
sa <- sample(200)
range(filearray[sa,sa,sa,53] - flazy[sa,sa,sa,53])


f <- normalizePath('~/Desktop/filearray_data/');
a <- subsetFM(f, list(1:287,1:200,1:601,1), c(287,200,601,1), 14L, NULL, TRUE);
filem <- filematrix::fm.open("~/Desktop/filearray_data/1", readonly = TRUE)
a <- as.vector(a)
b <- as.vector(filem[,1])
range(b-a)
bench::mark({
  a <- subsetFM(f, list(1:287,sample(1:200),1:601,1), c(287,200,601,1), 14L, NULL, TRUE);
}, {
  b <- filem[,1]
}, check = F) -> m; m$expression <- 1:2; m
*/
