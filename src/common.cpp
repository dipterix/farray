#include "common.h"

// Used to partition to sub-blocks
/**
 * BLOCKSIZE is used to partition sub-blocks
 * The unit is "element", default is 2048 to ensure the reading block is >= 8KB
 * to maximize reading speed
 * ub: Inf, lb: 1
 * It's best to set BLOCKSIZE to be 2048~16384. The default value is not optimal
 * because we want to calculate
 */
static R_xlen_t BLOCKSIZE = 2048;

/**
 * If sub-block size is too large, don't calculate indices (memory inefficient)
 * If element total memory size is greater than BLOCKLARGE per core, then no index
 * will be pre-calculated
 * The unit is byte, default is 2048^2, or 4MB index set/core (per core is to
 * handle forked process?).
 * In the default settings, a 4 core CPU allows 16MB index set, when 1 core (
 * this usually happens when forked process is used), then only 4MB cap per core
 *
 * The worst case is multisession on 8 cores: each session has 8 threads, then
 * total 64 * 4 MB = 256 MB. This is usually very bad so set threads=1 when
 * using multisession.
 *
 * On a server with 90 cores, this could use 32GB memories (???) So make sure
 * to mention that when writing paper farray_parallel should also make sure
 * to set threads
 *
 * ub: Inf, lb: 16, constraint, rounded to multiple of 16
 */
static R_xlen_t BLOCKLARGE = 4194304;

/**
 * BLOCKBUFFER, cap of memory buffer when reading large blocks
 * this is set to avoid memory overhead.
 * The unit is byte, default is FARRAY_BUFFERSIZE (64KB) and the cap is
 * FARRAY_BUFFERSIZE_LIMIT (1MB). It's not quite usual to set BLOCKBUFFER
 * as 64KB is already optimal (unless during the dev procedure)
 */
static R_xlen_t BLOCKBUFFER = FARRAY_BUFFERSIZE; // OMP buffer size

R_xlen_t setFArrayBlockSize(R_xlen_t size, R_xlen_t limit, R_xlen_t buf_size){
  if( size < 0 ){
    BLOCKSIZE = 2048;
  } else if( size > 0 ){
    BLOCKSIZE = size;
    if(BLOCKSIZE <= 1){
      BLOCKSIZE = 1;
    }
  }

  if( limit < 0 ) {
    BLOCKLARGE = 4194304;
  } else if ( limit > 0 ){
    // BLOCKLARGE = (limit - limit % 16);
    // if(BLOCKLARGE <= 16) {
    //   BLOCKLARGE = 16;
    // }
    BLOCKLARGE = limit;
  }
  if( buf_size < 0 ) {
    BLOCKBUFFER = FARRAY_BUFFERSIZE;
  } else if ( buf_size > 0 ){
    if(buf_size > FARRAY_BUFFERSIZE_LIMIT) {
      BLOCKBUFFER = FARRAY_BUFFERSIZE_LIMIT;
    } else {
      BLOCKBUFFER = (buf_size - buf_size % 16);
      if(BLOCKBUFFER < 16){
        BLOCKBUFFER = 16;
      }
    }
  }

  return BLOCKSIZE;
}

R_xlen_t getFArrayBlockSize(int which){
  if(which == 2){
    return (BLOCKBUFFER);
  }
  if(which == 1){
    return (BLOCKLARGE);
  }
  return BLOCKSIZE;
}

