#include "common.h"

// Used to partition to sub-blocks
static R_xlen_t BLOCKSIZE = 2048;
// If sub-block size is too large, don't calculate indices (memory inefficient)
// ~ 250 MB index set
static R_xlen_t BLOCKLARGE = 4194304; // 2048^2, 33.6 MB per core, total memory overhead is 268 MB for double float * 8 cores
static R_xlen_t BLOCKBUFFER = FARRAY_BUFFERSIZE; // OMP buffer size

R_xlen_t setFArrayBlockSize(R_xlen_t size, R_xlen_t limit, R_xlen_t buf_size){
  if( size < 0 ){
    BLOCKSIZE = 2048;
  } else if( size > 0 ){
    BLOCKSIZE = size;
  }

  if( limit < 0 ) {
    BLOCKLARGE = 4194304;
  } else if ( limit > 0 ){
    BLOCKLARGE = limit;
  }
  if( buf_size < 0 ) {
    BLOCKBUFFER = FARRAY_BUFFERSIZE;
  } else if ( buf_size > 0 ){
    if(buf_size > FARRAY_BUFFERSIZE_LIMIT) {
      BLOCKBUFFER = FARRAY_BUFFERSIZE_LIMIT;
    } else {
      BLOCKBUFFER = buf_size;
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

