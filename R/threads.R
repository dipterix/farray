
#' @title Set Number of Threads for `'farray'`
#' @description Set number of threads used by 'OpenMP'
#' @param nr_of_threads number of CPU cores to use, or `NULL` to
#' stay unchanged, default is `getOption('farray.nthreads')`
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


#' Schedule parallel processes for `farray`
#' @description Enable parallel processing, need `dipsaus` to be installed.
#' For `"callr"` strategy, please also install `future.callr`.
#' @param enabled whether multiple-process strategy is enabled
#' @param strategy strategies to apply, see [future::plan()] for
#' some of the details. For `"callr"` plan, please install package
#' @param workers number of 'CPU' cores to use.
#' `future.callr`.
#' @param workers positive integer or `"auto"`, number of 'CPU' to use.
#' The default value is `"auto"`, i.e. `future::availableCores()`
#' @param ... Further passed to [future::plan()]
#'
#' @export
farray_parallel <- function(
  strategy = c(
    'multisession', 'multicore',
    'multiprocess', 'cluster', 'remote', 'callr'),
  enabled = TRUE, workers = 'auto',
  ...
){

  options('farray.parallel.strategy' = FALSE)
  strategy <- match.arg(strategy)
  if(!has_dipsaus()){
    stop('Package dipsaus not detected. Please install.packages("dipsaus")')
  }

  if(isTRUE(workers == 'auto')){
    # get maximum available workers
    workers <- future::availableCores()
  }

  if(enabled){

    if(strategy == 'multicore'){
      dipsaus::make_forked_clusters(..., workers = workers)
    } else if(strategy == 'callr'){
      callr <- import_from('callr', package = 'future.callr')
      future::plan(callr, ..., workers = workers)
    } else {
      args <- list(...)
      tryCatch({
        future::plan(strategy, ..., workers = workers)
      }, error = function(e){
        do.call(future::plan, c(list(strategy), args))
      })
    }

  } else {
    future::plan('sequential')
  }

  invisible()
}

