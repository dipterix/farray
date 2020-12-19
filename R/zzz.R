
.onUnload <- function (libpath) {
  library.dynam.unload("farray", libpath)
}


.onLoad <- function(libname, pkgname){
  options('farray.parallel.strategy' = FALSE)
  options('farray.chunk_memory' = 80)

  ncores <- parallel::detectCores(logical = TRUE)
  options('farray.nthreads' = ncores)
  set_farray_threads(ncores, TRUE)
}

