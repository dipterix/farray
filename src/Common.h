#ifndef DIP_FARRAY_COMMON_H
#define DIP_FARRAY_COMMON_H

// Common header that's required by all (most) files

#include <Rcpp.h>

using namespace Rcpp;

#ifndef NA_INTEGER64
//undef NA_INTEGER64
#define NA_INTEGER64 LLONG_MIN
#endif // NA_INTEGER64

// Lazyarray subset_mode - No index
#ifndef LASUBMOD_NOIDX
#define LASUBMOD_NOIDX 2
#endif

#ifndef LASUBMOD_SINGLE
#define LASUBMOD_SINGLE 1
#endif

#ifndef LASUBMOD_MULTI
#define LASUBMOD_MULTI 0
#endif

#ifdef FARRAY_BUFFERSIZE
#undef FARRAY_BUFFERSIZE
#endif

#define FARRAY_BUFFERSIZE 65536

#ifdef FARRAY_BUFFERSIZE_LIMIT
#undef FARRAY_BUFFERSIZE_LIMIT
#endif

#define FARRAY_BUFFERSIZE_LIMIT 180224


#ifdef FARRAY_DEBUG
#undef FARRAY_DEBUG
#endif

/*
 * For array with dimension [287 x 200 x 601 x 84]
 * BLOCKSIZE decides the size of block to read into from partition file
 *
 * by default, this array will be split into 3 parts
 * [287 x 200 x 601 x 84] => [287 x 200] x [601 x 1] x [84]
 *
 * 84 is # of partition /files
 * for each file, read in sub chunk of length 287 x 200 (> BLOCKSIZE)
 * total number of chunks to read is 601 per file
 *
 * loc2idx3 calculates indices within each sub-chunks so that it's easy to find then once data is loaded
 *
 * However, of sub-block is too large, for example [1e30 x 5] matrix, sub-block size is 1e30, loc2idx3 generates too many
 * indices but the indices come with cost of memory (this means super large index set). We wish to calculate
 * indices on the fly. The boundary is set by BLOCKLARGE.
 *
 * If # of indices > BLOCKLARGE, then don't pre-generate indices
 *
 */

/**
 * For optimal block size:
 * https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-2000-server/cc938632(v=technet.10)?redirectedfrom=MSDN
 *
 * for windows,
 * reading buffer should be at least 8 KB, 64 KB is best (CPU cache?)
 *
 * For R element, integer (4B), double (8B), so the minimum should be 2048 elements?
 * -
 *
 */
// Used to partition to sub-blocks
static R_xlen_t BLOCKSIZE = 2048;
// If sub-block size is too large, don't calculate indices (memory inefficient)
// ~ 250 MB index set
static R_xlen_t BLOCKLARGE = 4194304; // 2048^2, 33.6 MB per core, total memory overhead is 268 MB for double float * 8 cores
static R_xlen_t BLOCKBUFFER = FARRAY_BUFFERSIZE; // OMP buffer size

const static int64_t INTEGER64_ONE = 1;
const static R_xlen_t INTEGER_XLEN_ONE = 1;

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
R_xlen_t setFArrayBlockSize(R_xlen_t size, R_xlen_t limit = 0, R_xlen_t buf_size = 0);

// [[Rcpp::export]]
R_xlen_t getFArrayBlockSize(int which = 0);


#endif // DIP_FARRAY_COMMON_H
