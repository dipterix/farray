#include "common.h"


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

