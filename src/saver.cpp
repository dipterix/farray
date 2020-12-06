#include <cstdio>
#include <Rcpp.h>
#include "common.h"
#include "utils.h"
#include "classIndexSchedule.h"
// #include "openMPInterface.h"
#include "binIo.h"
#include "rFunctions.h"
using namespace Rcpp;

void handle_eptr(std::exception_ptr eptr) // passing by value is ok
{
  try {
    if (eptr) {
      std::rethrow_exception(eptr);
    }
  } catch(const std::exception& e) {
    Rcout << "Caught exception \"" << e.what() << "\"\n";
  }
}

template <typename T>
int64_t subsetAssignFMtemplate(
    const std::string& rootPath, T* data, int64_t dLen, T na,
    const std::vector<int64_t>& dim, const ParsedIndex* subparsed
) {
  if(subparsed->subset_mode != LASUBMOD_MULTI){
    stop("C++: `subsetAssignFM`: invalid subscript mode.");
  }
  int element_size = sizeof(T);
  // const std::vector<int64_t> target_dimension = subparsed->target_dimension;
  // std::vector<std::pair<std::vector<int64_t>, bool>> location_indices = subparsed->location_indices;

  ScheduledIndex* schedule = subparsed->schedule;
  const std::vector<int64_t> target_dimension = subparsed->target_dimension;
  std::vector<int64_t> partition_index = schedule->partition_index;    // detailed partition indexes   - always exists
  std::vector<int64_t> schedule_index = schedule->schedule_index;      // indices to schedule run blocks
  int64_t block_length = schedule->block_length;                  // # elements in a block (full version) = prod(block_dimension)
  int64_t block_expected_length = schedule->block_expected_length;// # elements in a block (subset version) = length(block_schedule)
  int64_t block_ndims = schedule->block_ndims;                    // length(block dim)
  std::vector<std::pair<std::vector<int64_t>, bool>> block_location = schedule->block_location;
  std::vector<int64_t> block_prod_dim = schedule->block_prod_dim; // prod([1, block dim]), used to locate indices when block is too large to index
  // bool block_indexed = schedule->block_indexed;                   // whether block_schedule can be trusted
  int64_t block_schedule_start = schedule->block_schedule_start;
  // int64_t block_schedule_end = schedule->block_schedule_end;      // min, max of block_schedule
  std::vector<int64_t> block_schedule = schedule->block_schedule; // given a flattened block (full version), which indices to subset?

  int64_t block_size = std::accumulate(target_dimension.begin(), target_dimension.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());
  int64_t partition_length = std::accumulate(dim.begin(), dim.end()-1, INTEGER64_ONE, std::multiplies<int64_t>());

  // private vars
  int64_t fileIdx;
  std::string filePath;
  int64_t chunk_end, chunk_start, block_number, last_idx;
  RFileConn fcon = RFileConn();
  int64_t rest, sub_index, subblock_dim_ii, mod, tmp;
  T* ptr_val = data;
  int64_t dataOffset = 0;
  std::vector<int64_t>::iterator ptr_block_schedule;
  int64_t buffer_size = getFArrayBlockSize(2) / element_size;
  if(buffer_size > block_expected_length){
    buffer_size = block_expected_length;
  }
  if(buffer_size < 1024){
    buffer_size = 1024;
  }
  std::vector<T> buffer = std::vector<T>(buffer_size);
  int64_t buffer_count = 0;

  for(int64_t part_ii = 0; part_ii < partition_index.size(); part_ii++){

    // check file
    fileIdx = partition_index[part_ii];
    if(fileIdx == NA_REAL || fileIdx == NA_INTEGER64 || fileIdx <= 0){
      continue;
    }

    // file is valid?
    filePath = as_dirpath(rootPath) + std::to_string(fileIdx) + ".bmat";

    fcon.connect(filePath, "r+b", true);
    fcon.ensureLength(partition_length, element_size, Shield<SEXP>(wrap(na)));

    // file is valid, plan schedules
    for(int64_t schedule_ii = 0; schedule_ii < schedule_index.size(); schedule_ii++ ){

      // locate where the rows are in the file
      block_number = schedule_index[schedule_ii];

      if(block_number == NA_INTEGER64 || block_schedule_start <= 0){
        continue;
      }

      chunk_end = block_length * block_number;
      chunk_start = chunk_end - block_length;

      // block_size = block_expected_length * schedule_counts_per_part
      dataOffset = (block_size * part_ii + schedule_ii * block_expected_length) % dLen;
      ptr_val = data + dataOffset;


      if( false ){

        // elm_written = 0;
        // last_idx = -2;
        // fseek(fcon.conn, 0, SEEK_SET);
        //
        // // don't calculate index on the fly.
        // for(ptr_block_schedule = block_schedule.begin();
        //     ptr_block_schedule != block_schedule.end();
        //     ptr_block_schedule++)
        // {
        //   if(*ptr_block_schedule < block_schedule_start || *ptr_block_schedule == NA_REAL ||
        //      *ptr_block_schedule == NA_INTEGER64){
        //      // *ptr_res_private++ = na_value;
        //   } else {
        //     // sub_index is the location of element
        //     sub_index = *ptr_block_schedule - 1;
        //     if( sub_index != NA_REAL && sub_index != NA_INTEGER &&
        //         sub_index != NA_INTEGER64 && sub_index >= 0 ) {
        //       // avoid fseek
        //       sub_index += chunk_start;
        //       if(sub_index - last_idx != 1){
        //         fseek(fcon.conn, element_size * sub_index, SEEK_SET);
        //         // print(wrap(last_idx));
        //         last_idx = sub_index;
        //       } else {
        //         last_idx++;
        //       }
        //       fwriteLEndian(ptr_val, 1, fcon.conn);
        //       elm_written++;
        //     }
        //     if(elm_written >= FARRAY_BUFFERSIZE) {
        //       fflush(fcon.conn);
        //       elm_written = 0;
        //     }
        //
        //     dataOffset++;
        //     if(dataOffset >= dLen){
        //       dataOffset = 0;
        //       ptr_val = data;
        //     } else {
        //       ptr_val++;
        //     }
        //   }
        // }
      } else {
        // non-indexed (usually memory too big for index), index on the fly

        last_idx = -2;
        for(int64_t ii = 0; ii < block_expected_length; ii++){
          rest = ii;
          sub_index = 0;

          // locate sub_index in the block buffer; the real index in partition is chunk_start + sub_index
          for(int64_t di = 0; di < block_ndims; di++ ){

            if(sub_index == NA_INTEGER64 || sub_index == NA_REAL || sub_index < 0 ){
              sub_index = NA_INTEGER64;
              break;
            }
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

          if( sub_index == NA_INTEGER64 || sub_index < 0 ) {
            continue;
          }

          sub_index += chunk_start;

          // avoid seek
          if(sub_index - last_idx != 1 || buffer_count == buffer_size){
            // flush buffer_count elements ends with sub_index - buffer_count
            fcon.seek(element_size * (sub_index - buffer_count), "write", "start");
            if(buffer_count > 0){
              // write data
              if(!isLittleEndian()){
                // reverse endianess
                revEndian(&(buffer[0]), buffer_count);
              }
              fcon.writeRaw((Rbyte*)(&(buffer[0])), buffer_count * element_size);
            }
            // reset buffer
            buffer_count = 0;
            last_idx = sub_index;
            buffer[buffer_count++] = *ptr_val;
          } else {
            buffer[buffer_count++] = *ptr_val;
            last_idx++;
          }

          dataOffset++;
          if(dataOffset >= dLen){
            dataOffset = 0;
            ptr_val = data;
          } else {
            ptr_val++;
          }

        }


        if(buffer_count > 0 && last_idx + 1 >= buffer_count){
          // flush buffer_count elements ends with sub_index - buffer_count
          fcon.seek(element_size * (last_idx + 1 - buffer_count), "write", "start");
          if(!isLittleEndian()){
            // reverse endianess
            revEndian(&(buffer[0]), buffer_count);
          }
          fcon.writeRaw((Rbyte*)(&(buffer[0])), buffer_count * element_size);
        }

      }

    }

    fcon.close();

  }

  return 1;
}

// [[Rcpp::export]]
int64_t subsetAssignFM(const std::string& rootPath, SEXP listOrEnv,
                       const std::vector<int64_t>& dim, SEXP data) {
  if(dim.size() < 2){
    stop("Dimension size must >= 2");
  }
  ParsedIndex* subparsed = parseAndScheduleBlocks(listOrEnv, dim);

  int64_t dLen = Rf_xlength(data);
  int64_t re = -1;

  switch(TYPEOF(data)) {
  case(REALSXP):
    re = subsetAssignFMtemplate<double>(rootPath, REAL(data), dLen, NA_REAL, dim, subparsed);
    // try{
    // re = subsetAssignFMtemplate<double>(rootPath, REAL(data), dLen, NA_REAL, dim, subparsed);
    // } catch(...){
    //   std::exception_ptr eptr = std::current_exception(); // capture
    //   handle_eptr(eptr);
    // }
    delete subparsed;
    break;
  case(INTSXP):
    try{
      re = subsetAssignFMtemplate<int>(rootPath, INTEGER(data), dLen, NA_INTEGER, dim, subparsed);
    } catch(...){}
    delete subparsed;
    break;
  default:
    delete subparsed;
  stop("C++: `subsetAssignFM`: unsupported data type.");
  }

  return re;
}

/*** R
rootPath <- tempfile(); dir.create(rootPath); rootPath <- normalizePath(rootPath)
listOrEnv = list(1:4, 2:3, 1); dim = c(4, 3 ,2); data = array(1:24, dim)[1:4, 2:3, 1]
subsetAssignFM(rootPath, listOrEnv, dim, as.double(data))
readBin(file.path(rootPath, '1.bmat'), 'double', n = prod(dim) + 2, size = 8L)
farray:::subsetFM(rootPath, list(1:4, 1:3, 1), dim, 14L, NULL, FALSE)
farray:::subsetFM(rootPath, list(), dim, 14L, NULL, FALSE)
*/
