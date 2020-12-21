#' Product of two arrays
#' @description Consistent with `tensor()` function in `tensor` package, or
#' `ttl()` in `rTensor` package.
#' @param A,B an [`array`] or [`farray`]
#' @param alongA,alongB integers; margins in `A` and `B` to be collapsed.
#' @param path,meta_name where to store the results; see [`farray`].
#' @details It is highly recommended that `A` is a small array and `B` is a
#' large array. The performance if highly related to how `alongB` is assigned.
#' If all the fast margins (see [`is_fast_margin`]) are contained within
#' `alongB`, then there is no memory optimization, and the whole calculation
#' occurs in the memory. If at least one fast margin is not in `alongB`, then
#' the calculation is memory optimized.
#'
#' @return A [`farray`] object.
#'
#' @examples
#'
#' x <- matrix(1:12, ncol = 4)
#' y <- array(rnorm(120), c(4,5,6))
#'
#' # Equivalent to tensor::tensor(x, y, alongA = 2, alongB = 1)
#' # or rTensor::ttm(rTensor::as.tensor(y), x, m = 1)
#' tensorprod(x, y, alongA = 2, alongB = 1)
#'
#' @export
tensorprod <- function(A, B, alongA = integer(0), alongB = integer(0),
                       path = tempfile(), meta_name = 'farray.meta'){
  dimA <- dim(A)
  dimB <- dim(B)
  if(!length(dimA)){
    dimA <- c(length(A), 1)
  }
  if(!length(dimB)){
    dimB <- length(B)
  }

  # check along dimension match
  alongDimA <- dimA[alongA]
  alongDimB <- dimB[alongB]

  # keep dimensions
  keepA <- negative_subscript2(seq_along(dimA), alongA)
  keepB <- negative_subscript2(seq_along(dimB), alongB)

  nalong <- length(alongDimA)

  if(nalong != length(alongDimB)) {
    stop('Mismatch in "along" dimensions')
  }

  if(nalong && sum((alongDimA - alongDimB)^2) > 0) {
    stop('Mismatch in "along" dimensions')
  }

  # calculate final dimension
  targetDim1 <- negative_subscript2(dimA, alongA)
  targetDim2 <- negative_subscript2(dimB, alongB)
  if(!length(targetDim1)){ targetDim1 <- 1 }
  if(!length(targetDim2)){ targetDim2 <- 1 }
  targetDim <- c(targetDim1, targetDim2)

  dest <- farray(path = path, dim = targetDim, storage_format = 'double',
                 read_only = FALSE, meta_name = meta_name)

  has_results <- FALSE

  apermA <- c(alongA, negative_subscript2(seq_along(dimA), alongA))
  apermB <- c(alongB, negative_subscript2(seq_along(dimB), alongB))


  # check block index
  isBlockIdx <- sapply(keepB, function(idx){ is_fast_margin(dimB, idx) })
  if(length(isBlockIdx) && any(isBlockIdx)) {
    # use this margin to index slice of B
    partition_margin <- max(keepB[isBlockIdx])
    locList <- replicate(length(dimB), get_missing_value(), simplify = FALSE)
    getSliceB <- function(idx){
      locList[[partition_margin]] <- idx
      expr <- as.call(c(list(quote(`[`), quote(B), drop=FALSE), locList))
      eval(expr)
    }
    margin_size <- dimB[[partition_margin]]

    apermedA <- aperm(A[drop=FALSE], apermA)
    dim(apermedA) <- c(prod(dimA[alongA]), prod(negative_subscript2(dimA, alongA)))

    dimSliceB <- dimB
    dimSliceB[[partition_margin]] <- 1

    lapply(seq_len(margin_size), function(idx){
      sliceB <- getSliceB(idx)
      apermedB <- aperm(sliceB, apermB)
      dim(apermedB) <- c(prod(dimSliceB[alongB]), prod(negative_subscript2(dimSliceB, alongB)))
      dest$set_partition_data(idx, crossprod(apermedA, apermedB))
      return()
    })

  } else {

    # sadly, non of the index in B is block index, which means
    # our indexing method has no benefit. In this case, just get all
    # the data into memory
    # Users should avoid this case by permutate the array
    # when storing.

    apermedA <- aperm(A[drop=FALSE], apermA)
    dim(apermedA) <- c(prod(dimA[alongA]), prod(negative_subscript2(dimA, alongA)))

    apermedB <- aperm(B[drop=FALSE], apermB)
    dim(apermedB) <- c(prod(dimB[alongB]), prod(negative_subscript2(dimB, alongB)))

    dest[] <- crossprod(apermedA, apermedB)

  }

  return(dest)

  # # case 1: alongB does not contain the last dim B
  # if(!length(dimB) %in% alongB) {
  #   B <- as.farray(B)
  #   orderA <- order(alongA)
  #   alongA <- alongA[orderA]
  #   alongB <- alongB[orderA]
  #   apermA <- c(alongA, negative_subscript2(seq_along(dimA), alongA))
  #   apermB <- c(alongB, negative_subscript2(seq_along(dimB), alongB))
  #   dimSliceB <- B$partition_dim()
  #
  #   if(FALSE && inherits(A, "AbstractFArray")) {
  #
  #     # check whether subset A is a good idea
  #     parsed <- parseAndScheduleBlocks2(lapply(dimA, function(d){1}), dimA)
  #     if(parsed$schedule$block_indexed){
  #       index_marginA <- which(!seq_along(dimA) %in% c(alongA, seq_len(parsed$schedule$block_ndims)))
  #       if(length(index_marginA)) {
  #         index_marginA <- max(index_marginA)
  #         # Slice along index_marginA
  #         locList <- replicate(length(dimA), get_missing_value(), simplify = FALSE)
  #         getSliceA <- function(idx){
  #           locList[[index_marginA]] <- idx
  #           expr <- as.call(c(list(quote(`[`), quote(A)), locList, list(drop=FALSE)))
  #           eval(expr)
  #         }
  #
  #         sliceADim <- dimA
  #         sliceADim[[index_marginA]] <- 1
  #         index_sizeA <- dimA[[index_marginA]]
  #         lapply(seq_len(B$npart), function(partB){
  #           sliceB <- aperm(B$get_partition_data(partB), apermB)
  #           dim(sliceB) <- c(prod(dimSliceB[alongB]), prod(dimSliceB[-alongB]))
  #
  #           sliceProd <- sapply(seq_len(index_sizeA), function(partA){
  #             sliceA <- getSliceA(partA)
  #             apermedA <- aperm(sliceA, apermA)
  #             dim(apermedA) <- c(prod(sliceADim[alongA]), prod(sliceADim[-alongA]))
  #             crossprod(apermedA, sliceB)
  #           }, simplify = TRUE)
  #
  #           partialDimB <- dimSliceB[-alongB]
  #           partialDimA <- dimA[-alongA]
  #           idxpartialA <- index_marginA - sum(alongA < index_marginA)
  #           dim(sliceProd) <- c(partialDimA[-idxpartialA], partialDimA[idxpartialA], prod(partialDimB))
  #           ndims <- length(dim(sliceProd))
  #           order <- c(seq_len(idxpartialA-1), ndims - 1)
  #           order2 <- seq_len(ndims)
  #           order2 <- order2[!order2 %in% order]
  #           order <- c(order, order2)
  #           sliceProd <- aperm(sliceProd, order)
  #           dest$set_partition_data(part = partB, data = sliceProd)
  #           return(NULL)
  #         })
  #         has_results <- TRUE
  #       }
  #     }
  #
  #   }
  #
  #   if(!has_results) {
  #     apermedA <- aperm(A[], apermA)
  #     dim(apermedA) <- c(prod(dimA[alongA]), prod(negative_subscript2(dimA, alongA)))
  #
  #     lapply(seq_len(B$npart), function(partB){
  #
  #       sliceB <- aperm(B$get_partition_data(partB), apermB)
  #       dim(sliceB) <- c(prod(dimSliceB[alongB]), prod(negative_subscript2(dimSliceB, alongB)))
  #       sliceProd <- crossprod(apermedA, sliceB)
  #       dest$set_partition_data(part = partB, data = sliceProd)
  #       return(NULL)
  #     })
  #     has_results <- TRUE
  #   }
  #   return(dest)
  # } else {
  #   # case 2: alongB contains last dim of B
  #   sel <- alongB != length(dimB)
  #   alongB_tmp <- alongB[sel]
  #   alongA_tmp <- alongA[sel]
  #   tmp <- Recall(A, B, alongB_tmp, alongA_tmp, tempfile())
  #   dest <- collapse2(tmp, c(alongA[!sel], tmp$ndim), method = 'sum', path = path)
  # }



  return(dest)

}
