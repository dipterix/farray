#' @title Create or load a \code{farray} instance
#' @author Zhengjia Wang
#'
#' @description Creates or load a \code{farray} that stores data on the hard
#' disks. The data content is load on demand.
#'
#' @param path path to a local drive where array data should be stored
#' @param x An R matrix or array
#' @param dim integer vector, dimension of array, see \code{\link{dim}}
#' @param storage_format data type, choices are \code{"double"},
#' \code{"integer"}, \code{"character"}, and \code{"complex"}; see details
#' @param read_only whether created array is read-only
#' @param meta_name header file name, default is \code{"farray.meta"}
#' @param ... passed into \code{farray}
#'
#' @return An \code{R6} class of \code{farray}. The class name is
#' \code{FileArray}, inherits \code{AbstractFArray}.
#'
#' @details The function \code{farray()} can either create or load an array
#' on the hard drives. When \code{path} exists as a directory, and there is
#' a valid array instance stored, \code{farray} will ignore other parameters
#' such as \code{storage_format}, \code{type}, and sometimes \code{dim} (see
#' Section "Array Partitions"). The function will try to load the existing array
#' given by the descriptive meta file. When \code{path} is missing or there is
#' no valid array files inside of the directory, then a new array will be
#' spawned, and \code{path} will be created automatically if it is missing.
#'
#' The argument \code{meta_name} specifies the name of file which stores
#' all the attribute information such as the total dimension, partition size,
#' file format, and storage format etc. There could be multiple meta files for
#' the same array object; see Section "Array Partitions" for details.
#'
#' @section Performance:
#'
#' Type \code{filearray} stores data in its binary form "as-is" to the local
#' drives. This format is compatible with the package \code{filematrix}.
#' The data types supported are integers and double-float numbers.
#'
#' Type \code{fstarray} stores data in \code{fst} format defined by the
#' package \code{fstcore} using 'ZSTD' compression technique. Unlike
#' \code{filearray}, \code{fstarray} supports complex numbers and string
#' characters in addition to integer and double numbers.
#'
#' The performance on solid-state drives mounted on 'NVMe' shows
#' \code{filearray} can reach up to 3 GB per second for reading speed and
#' \code{fstarray} can reach up to 1 GB per second.
#'
#' By default, \code{filearray} will be used if the storage format is supported,
#' and \code{fstarray} is the back-up option. However, if the array data is
#' structured or ordered, or the storage size is a major concern,
#' \code{fstarray} might achieve a better performance because it compresses
#' data before writing to hard drive.
#'
#' To explicitly create file array, use the function \code{filearray()}.
#' Similarly, use \code{fstarray()} to create \code{fst}-based array.
#'
#' @section Array Partitions:
#'
#' A \code{farray} partitions data in two ways: file partitions and in-file
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
#' \eqn{2400000 x 100}, you can call \code{farray} with the new dimension (
#' see examples). Please make sure the \code{type} and \code{meta_name} are
#' specified.
#'
#' 2. In-file Blocks:
#'
#' Within each file, the data are stored in blocks. When reading the data, if
#' an element within each block is used, then the whole block gets read.
#'
#' For \code{filearray}, the block size equals to the first margin. For
#' example, a \eqn{100 x 200 x 3} file array will have 3 file partitions,
#' 200 blocks, each block has 100 elements
#'
#' As for \code{fstarray}, the lower bound of block size can be set by
#' \code{options(farray.fstarray.blocksize=...)}. By default, this number is
#' 16,384. For a \eqn{100 x 200 x 3} array, each partition only has one block
#' and block number if 20,000.
#'
#' @section Indexing and Recommended Dimension Settings:
#'
#' If there is a dimension that defines the unit of analysis, then make it the
#' last margin index. If a margin is rarely indexed, put it in the first margin.
#' This is because indexing along the last margin is the fastest, and indexing
#' along the first margin is the slowest.
#'
#' If \code{x} has \eqn{200 x 200 x 200} dimension, \code{x[,,i]} is the
#' fastest, then \code{x[,i,]}, then \code{x[i,,]}.
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

  call <- match.call()

  if(!storage_format %in% c("integer", "double")){
    stop("FileArray only support `integer` or `double` data types.")
  }

  call[[1]] <- quote(filearray)
  eval(call)
}

#' @rdname farray
#' @export
filearray <- function(
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
as.farray.AbstractFArray <- function(x, path, ...){
  x
}

