is_block_index <- function(dim, idx){
  if(idx == length(dim)){
    return(TRUE)
  }
  parsed <- parseAndScheduleBlocks2(lapply(dim, function(d){1}), dim)
  if(parsed$schedule$block_indexed){
    return(parsed$schedule$block_ndims < idx)
  }
  return(FALSE)
}

farray_collapse <- function(x, along, method = c("sum", "average"), path = tempfile()){
  method <- match.arg(method)
  average <- method == "average"
  xdim <- dim(x)
  ndims <- length(xdim)
  keep <- seq_len(ndims)
  keep <- keep[!keep %in% along]

  stopifnot(length(along) > 0)

  if(!length(keep)){
    if(average){
      return(mean(x))
    } else {
      return(sum(x))
    }
  }

  # check the largest idx
  slice_margin <- keep[[length(keep)]]

  collapse_margin <- !is_block_index(xdim, slice_margin)
  if(collapse_margin){
    keep <- c(keep, length(xdim))
    slice_margin <- ndims
    tpath <- tempfile()
    on.exit({
      if(dir.exists(tpath)){
        unlink(tpath, recursive = TRUE, force = TRUE)
      }
    }, after = TRUE, add = TRUE)
  } else {
    tpath <- path
  }

  desc_dim <- xdim[keep]

  if(has_dipsaus()){
    collapse_func <- function(slice, keep){
      dipsaus::collapse(slice, keep, average = average)
    }
  } else if (average){
    collapse_func <- function(slice, keep){
      apply(slice, keep, mean)
    }
  } else {
    collapse_func <- function(slice, keep){
      apply(slice, keep, sum)
    }
  }

  # in favor of this slice type
  loc <- replicate(ndims, get_missing_value(), simplify = FALSE)
  getSlice <- function(idx){
    loc[[slice_margin]] <- idx
    expr <- as.call(c(list(quote(`[`), quote(x), drop=FALSE), loc))
    eval(expr)
  }

  if(length(desc_dim) == 1){
    # no need to collapse_margin
    res <- sapply(seq_len(xdim[slice_margin]), function(ii){
      slice <- getSlice(ii)
      collapse_func(slice, keep)
    }, simplify = TRUE)
    desc <- as.farray(res, path, dim = c(length(res), 1), storage_format = 'double')
  } else {
    if(length(desc_dim) != ndims){
      desc <- farray(tpath, dim = desc_dim, read_only = FALSE, storage_format = 'double')

      lapply(seq_len(xdim[slice_margin]), function(ii){
        slice <- getSlice(ii)
        desc$set_partition_data(ii, collapse_func(slice, keep))
      })

    } else {
      desc <- x
    }

    if(collapse_margin){
      res <- 0
      for(ii in seq_len(desc$npart)){
        res <- res + desc$get_partition_data(part = ii)
      }
      if(length(desc_dim) > 2){
        dim(res) <- desc_dim[-length(desc_dim)]
      }
      if(average){
        res <- res / desc$npart
      }
      desc <- as.farray(res, path = path, storage_format = 'double')
    }
  }


  desc

}

negative_subscript2 <- function(x, sub){
  if(length(sub)){
    return(x[-sub])
  } else {
    return(x)
  }
}

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
  isBlockIdx <- sapply(keepB, function(idx){ is_block_index(dimB, idx) })
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
  #   dest <- farray_collapse(tmp, c(alongA[!sel], tmp$ndim), method = 'sum', path = path)
  # }



  return(dest)

}
