#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom yaml read_yaml
#' @importFrom yaml write_yaml
#' @import utils
#' @import stats
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' @importFrom methods signature
#' @useDynLib farray, .registration = TRUE
NULL

#' Check if Package \code{'dipsaus'} has been installed
#' @export
has_dipsaus <- function(){
  system.file('', package = 'dipsaus') != ''
}

import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' function: %s(). Please check whether package `%s` is installed.", package, name, package))
  }
}

#' @title Set Number of Threads for \code{'farray'}
#' @description Set number of threads used by 'OpenMP'
#' @param nr_of_threads number of CPU cores to use, or \code{NULL} to
#' stay unchanged, default is \code{getOption('farray.nthreads')}
#' @param reset_after_fork whether to reset after forked process
#' @param max whether return maximum available threads
#' @return Number of cores currently used.
#' @name farray-threads
#' @export
set_farray_threads <- function(nr_of_threads = getOption('farray.nthreads'), reset_after_fork = NULL){
  if(!is.null(reset_after_fork)){
    reset_after_fork <- isTRUE(reset_after_fork)
  }
  if(is.null(nr_of_threads) || !is.numeric(nr_of_threads) || nr_of_threads == 0){
    nr_of_threads <- future::availableCores()
  }
  nr_of_threads = max(nr_of_threads, 1)
  setFArrayThread(n = nr_of_threads, reset_after_fork = reset_after_fork)
  getFArrayThread()
}

#' @rdname farray-threads
#' @export
get_farray_threads <- function(max = FALSE){
  getFArrayThread(max = isTRUE(max))
}

get_missing_value <- function(){
  (function(...){
    parseDots(environment(), FALSE)[[1]]
  })(,)
}


rand_string <- function(length = 50){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}


#' Automatically remove array data
#' @author Zhengjia Wang
#' @description Remove the files containing array data once no
#' 'farray' instance is using the folder. Require
#' installation of \code{dipsaus} package (at least version 0.0.8).
#' @param x 'farray' instance
#' @param onexit passed to \code{\link{reg.finalizer}}
#'
#' @details \code{auto_clear_farray} attempts to remove the entire folder
#' containing array data. However, if some files are not created by the
#' array, only partition data and meta file will be removed, all the
#' artifacts will remain and warning will be displayed. One exception is
#' if all files left in the array directory are \code{*.meta} files,
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
