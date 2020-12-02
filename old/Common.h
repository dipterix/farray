#ifndef DIP_FARRAY_COMMON_H
#define DIP_FARRAY_COMMON_H

#include <vector>

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

#ifdef FARRAY_DEBUG
#undef FARRAY_DEBUG
#endif

const static int64_t INTEGER64_ONE = 1;


/*
 * farray dimension is [in-file dim (iDim)] x [file dim (fDim)]
 *
 * 1. block size: when reading an element from a block, the whole block gets read
 * 2. block dim (bDim): block dimension within a file
 * 3. in-file dim: the dimensions within a partition
 * 4. file dim: total length is the file length
 *
 * iDimSize: length(iDim), > 0 (>= 1)
 * bDimSize: length(bDim) = length(iDim)
 * fDimSize: length(fDim), > 0 (>= 1)
 *
 * iLength: prod(iDim): in-file actual length
 * bLength: prod(bDim): block length (also the single buffer length)
 * fLength: prod(fDim): total partition length
 * iLength2: in-file storage length (inflated, see below, multiple of buffer length)
 *
 * in-file length is rounded up to multiply of block dim
 *
 * ## Example:
 * For array with dimension [iDim = 287 x 200] x [fDim = 601 x 84]
 * If block dim is [16 x 32],
 *
 * Within partition can store [288 x 224] = [16*18  x 32*7] elements
 *
 * Unlike farray, *always* index arrays
 *
 * bufferSize = Single buffer size = bLength
 * total buffer size is bufferSize * nThreads (# of threads)
 *
 */


#endif // DIP_FARRAY_COMMON_H
