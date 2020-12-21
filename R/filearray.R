#' @title Create or load a `farray` instance
#' @author Zhengjia Wang
#'
#' @description Creates or load a `farray` that stores data on the hard
#' disks. The data content is load on demand.
#'
#' @param path path to a local drive where array data should be stored
#' @param x An R matrix or array
#' @param dim integer vector, dimension of array, see [dim()]
#' @param storage_format data type, choices are `"double"`,
#' `"integer"`, `"character"`, and `"complex"`; see details
#' @param read_only whether created array is read-only
#' @param meta_name header file name, default is `"farray.meta"`
#' @param ... passed into `farray`
#'
#' @return An `R6` class of `farray`. The class name is
#' `FileArray`, inherits `AbstractFArray`.
#'
#' @details The function `farray()` can either create or load an array
#' on the hard drives. When `path` exists as a directory, and there is
#' a valid array instance stored, `farray` will ignore other parameters
#' such as `storage_format`, `type`, and sometimes `dim` (see
#' Section "Array Partitions"). The function will try to load the existing array
#' given by the descriptive meta file. When `path` is missing or there is
#' no valid array files inside of the directory, then a new array will be
#' spawned, and `path` will be created automatically if it is missing.
#'
#' The argument `meta_name` specifies the name of file which stores
#' all the attribute information such as the total dimension, partition size,
#' file format, and storage format etc. There could be multiple meta files for
#' the same array object; see Section "Array Partitions" for details.
#'
#' @section Performance:
#'
#' Type `filearray` stores data in its binary form "as-is" to the local
#' drives. This format is compatible with the package `filematrix`.
#' The data types supported are integers and double-float numbers.
#'
#' The performance on solid-state drives mounted on 'NVMe' shows
#' `filearray` can reach up to 1-3 GB per second for reading speed.
#'
#' @section Array Partitions:
#'
#' A `farray` partitions data in two ways: file partitions and in-file
#' blocks.
#'
#' 1. File-level Partition:
#'
#' The number of file partitions matches with the last array margin.
#' Given a \eqn{100 x 200 x 30 x 4} array, there will be 4 partitions, each
#' partition stores a slice of data containing a \eqn{100 x 200 x 30}
#' sub-array, or \eqn{2,400,000} elements.
#'
#' Once an array is created, the length of each partition does not change
#' anymore. However, the shape of each partition can be changed. The number of
#' partitions can grow or trim. To change these, you just need to create a
#' new meta file and specify the new dimension at no additional cost. Use
#' the previous example. The partition sub-dimension can be
#' \eqn{10000 x 60}, \eqn{2000 x 300}, or \eqn{1000 x 200 x 3} as
#' long as the total length matches. The total partitions can change to
#' 3, 5, or 100, or any positive integer. To change the total dimension to
#' \eqn{2400000 x 100}, you can call `farray` with the new dimension (
#' see examples). Please make sure the `type` and `meta_name` are
#' specified.
#'
#' 2. In-file Blocks:
#'
#' Within each file, the data are stored in blocks. When reading the data, if
#' an element within each block is used, then the whole block gets read.
#'
#' For `filearray`, the block size equals to the first margin. For
#' example, a \eqn{100 x 200 x 3} file array will have 3 file partitions,
#' 200 blocks, each block has 100 elements
#'
#' @section Indexing and Recommended Dimension Settings:
#'
#' If there is a dimension that defines the unit of analysis, then make it the
#' last margin index. If a margin is rarely indexed, put it in the first margin.
#' This is because indexing along the last margin is the fastest, and indexing
#' along the first margin is the slowest.
#'
#' If `x` has \eqn{200 x 200 x 200} dimension, `x[,,i]` is the
#' fastest, then `x[,i,]`, then `x[i,,]`.
#'
#' @examples
#'
#' library(farray)
#'
#' path <- tempfile()
#'
#' # ---------------- case 1: Create new array ------------------
#' arr <- farray(path, storage_format = 'double', dim = c(2,3,4))
#' arr[] <- 1:24
#'
#' # Subset and get the first partition
#' arr[,,1]
#'
#' # Partition file path (total 4 partitions)
#' arr$get_partition_fpath()
#'
#' # Removing array doesn't clear the data
#' rm(arr); gc()
#'
#' # ---------------- Case 2: Load from existing directory ----------------
#' # Load from existing path, no need to specify other params
#' arr <- farray(path, read_only = TRUE)
#'
#' summary(arr, quiet = TRUE)
#'
#' # ---------------- Case 3: Import from existing data ----------------
#'
#' # Change dimension to 6 x 20
#'
#' arr1 <- farray(path, dim = c(6,20), meta_name = "arr_6x20.meta")
#'
#' arr1[,1:5]
#'
#' arr1[,1:6] <- rnorm(36)
#'
#' # arr also changes
#' arr[,,1]
#'
#'
#' # ---------------- Case 4: Converting from R arrays ----------------
#'
#' x <- matrix(1:16, 4)
#' x <- as.fmatrix(x)
#' x[,]  # or x[]
#'
#'
#'
#' @export
farray <- function(
  path, dim, read_only = FALSE,
  storage_format = 'double',
  meta_name = 'farray.meta'){
  if (missing(dim)) {
    re <-
      FileArray$new(
        path = path ,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  } else {
    re <-
      FileArray$new(
        path = path ,
        dim = dim,
        storage_format = storage_format,
        read_only = !isFALSE(read_only),
        meta_name = meta_name
      )
  }
  if (re$can_write) {
    re$save_meta()
  }
  re
}

#' @rdname farray
#' @export
filearray <- farray

#' @rdname farray
#' @export
as.fmatrix <- function(x, ...){
  UseMethod("as.fmatrix")
}

#' @export
as.fmatrix.default <- function(x, ...){
  re <- as.farray(x, ...)
  as.fmatrix.AbstractFArray(re)
}

#' @export
as.fmatrix.AbstractFArray <- function(x, ...){
  x <- x$clone()
  x$make_readonly()
  dim(x) <- c(x$partition_length, x$npart)
  x
}


#' @rdname farray
#' @export
as.farray <- function(x, path, ...){
  UseMethod("as.farray")
}

#' @export
as.farray.default <- function(x, path, dim, storage_format, ...){

  if(missing(path)){
    path <- tempfile()
  }
  if(missing(dim)){
    dim <- attr(x, "dim")
    if(!length(dim)){
      dim <- c(length(x), 1)
    }
  }
  stopifnot(length(dim) >= 2)

  if(missing(storage_format)){
    storage_format <- storage.mode(x)
  }


  re <- farray(path, dim = dim, storage_format = storage_format, ...)
  dim(x) <- dim
  re[] <- x
  re

}


#' @export
as.farray.AbstractFArray <- function(x, path, dim, partitions, meta_name = "farray.meta", ...){
  if(missing(path) && missing(partitions)){
    if(!missing(dim)){
      x <- x$clone(deep = FALSE)
      dim(x) <- dim
    }
    return(x)
  }

  if(missing(path)){
    path <- tempfile()
  }

  if(missing(partitions)) {
    partitions <- seq_len(x$npart)
  }

  if(missing(dim)){
    dim <- x$partition_dim()
    dim[[length(dim)]] <- length(partitions)
  } else {
    stopifnot(dim[[length(dim)]] == length(partitions))
    stopifnot(prod(dim[-length(dim)]) == x$partition_length)
  }

  re <- farray(path, dim = dim, read_only = FALSE, storage_format = x$storage_format, meta_name = meta_name)

  # copy everything as symlink to path
  for(ii in seq_along(partitions)){
    p <- partitions[[ii]]
    fs <- c(
      x$get_partition_fpath(p, full_path = FALSE),
      x$get_partition_fpath(p, full_path = FALSE, type = "desc"),
      x$get_partition_fpath(p, full_path = FALSE, summary_file = TRUE)
    )
    for(f in fs){
      source <- file.path(x$storage_path, f)
      tname <- sub("^[0-9]+", ii, f)
      target <- file.path(path, tname)
      if(file.exists(target)){
        unlink(target)
      }
      file.symlink(source, target)
    }
  }

  re
}

#' @export
as.farray.data.frame <- function(x, path, dim, storage_format, ...){
  if(missing(path)){
    path <- tempfile()
  }
  if(!all(sapply(x, mode) == "numeric")){
    stop(sQuote("x"), " must be a numerical data.frame.")
  }
  ncols <- ncol(x)
  if(missing(storage_format)){
    if(length(ncols) && all(sapply(x, storage.mode) == 'integer')){
      storage_format <- "integer"
    } else {
      storage_format <- 'double'
    }
  }
  if(missing(dim)){
    dim <- base::dim(x)
  } else if(dim[[length(dim)]] != ncols || prod(dim[-length(dim)]) != nrow(x)){
    stop("Dimension not match.")
  }

  re <- farray(path, dim = dim, storage_format = storage_format, ...)
  for(ii in seq_len(ncols)){
    re$set_partition_data(part = ii, data = x[[ii]])
  }
  re
}

#' @export
as.farray.fst_table <- function(x, path, dim, storage_format, ...){
  if(missing(path)){
    path <- tempfile()
  }
  meta <- .subset2(x, "meta")
  if(!all(meta$columnTypes %in% c(5,10))){
    stop(sQuote("x"), " must be a numerical data.frame.")
  }
  ncols <- ncol(x)
  if(missing(storage_format)){
    if(length(ncols) && all(meta$columnTypes == 5)){
      storage_format <- "integer"
    } else {
      storage_format <- 'double'
    }
  }
  if(missing(dim)){
    dim <- base::dim(x)
  } else if(dim[[length(dim)]] != ncols || prod(dim[-length(dim)]) != nrow(x)){
    stop("Dimension not match.")
  }

  re <- farray(path, dim = dim, storage_format = storage_format, ...)
  lapply2(seq_len(ncols), function(ii){
    re$set_partition_data(part = ii, data = x[[ii]])
  })
  # for(ii in seq_len(ncols)){
  #   re$set_partition_data(part = ii, data = x[[ii]])
  # }
  re
}


#' Automatically remove array data
#' @author Zhengjia Wang
#' @description Remove the files containing array data once no
#' 'farray' instance is using the folder. Require
#' installation of `dipsaus` package (at least version 0.0.8).
#' @param x 'farray' instance
#' @param onexit passed to [reg.finalizer()]
#'
#' @details `auto_clear_farray` attempts to remove the entire folder
#' containing array data. However, if some files are not created by the
#' array, only partition data and meta file will be removed, all the
#' artifacts will remain and warning will be displayed. One exception is
#' if all files left in the array directory are `*.meta` files,
#' all these meta files will be removed along with the folder.
#'
#' @examples
#'
#' path <- tempfile()
#' arr_dbl <- farray(path, storage_format = 'double',
#'                      dim = 2:4, meta_name = 'meta-dbl.meta')
#' arr_dbl[] <- 1:24
#' auto_clear_farray(arr_dbl)
#'
#' arr_dbl2 <- farray(path, storage_format = 'double',
#'                      dim = 2:4, meta_name = 'meta-2.meta')
#' auto_clear_farray(arr_dbl2)
#'
#' # remove either one, the directory still exists
#' rm(arr_dbl); invisible(gc(verbose = FALSE))
#'
#' arr_dbl2[1,1,1]
#'
#' # Remove the other one, and path will be removed
#' rm(arr_dbl2); invisible(gc(verbose = FALSE))
#'
#' dir.exists(path)
#' arr_check <- farray(path, storage_format = 'double',
#'                        dim = 2:4, meta_name = 'meta-2.meta')
#'
#' # data is removed, so there should be no data (NAs)
#' arr_check[]
#'
#' @export
auto_clear_farray <- function(x, onexit = FALSE){
  if(requireNamespace('dipsaus', quietly = TRUE)){
    path <- x$storage_path
    path <- normalizePath(path)
    dipsaus::shared_finalizer(x, key = path, function(e){
      e$remove_data(force = TRUE)
    }, onexit = onexit)
    rm(path)
  }
  rm(x, onexit)
  invisible()
}

#' Check if indexing along a margin is fast
#' @param dim dimension of an array, integers
#' @param margin integers, which margins to check
#' @return A logical vector.
#' @details A [farray] object has fast indexing margin and slow indexing margin.
#' Indexing along fast margins is speed optimized, while indexing within along
#' the slow margins is always discouraged. When the array is extremely large,
#' subset along slow margins could hurt the total performance. See examples for
#' profiling.
#'
#' Here's how slow margins are calculated. An internal "block size" is set to
#' maximize the disk reading speed. The default block size is 2048. All margins
#' with cumulative product of dimension greater than the block size is
#' considered fast. The last margin is always fast, and the first margin is
#' always slow. For example, a `100x200x300` array has only one fast margin (3)
#' because the first two elements of `cumprod(c(100,200,300))` are less equal
#' than 2048. A `100x200` array has one fast margin and one slow margin. Though
#' the first two elements of `cumprod(c(100,200))` are less equal than 2048, the
#' second margin, which is the last margin is always fast.
#'
#' @examples
#'
#' is_fast_margin(c(100,200,300))
#' is_fast_margin(c(100,200))
#'
#' # First two are slow
#' is_fast_margin(c(2048,2,2,2))
#'
#' # Only the first margin is slow
#' is_fast_margin(c(2049,2,2,2))
#'
#' if(interactive()) {
#'   # the last two margins are fast margins
#'   x <- as.farray(rnorm(2^24), dim = c(64,64,64,64))
#'   is_fast_margin(dim(x))
#'
#'   # fast
#'   system.time({ x[,,,1] })
#'   system.time({ x[,,1,] })
#'
#'   # slow
#'   system.time({ x[,1,,] })
#'   system.time({ x[1,,,] })
#' }
#'
#' @export
is_fast_margin <- function(dim, margin){
  if(missing(margin)){
    margin <- seq_along(dim)
  }
  parsed <- parseAndScheduleBlocks2(lapply(dim, function(d){1}), dim)
  res <- parsed$schedule$block_ndims < margin
  res[margin == length(dim)] <- TRUE
  return(res)
}
