#include "openMPInterface.h"


int getFArrayThread(bool max){
#ifdef _OPENMP
  if(detectFork){
    return 1;
  }
  if( max ){
    return omp_get_max_threads();
  }
  int t = farrayThreads < 0 ? omp_get_max_threads() : std::min(farrayThreads, omp_get_max_threads());
  return std::max(t, 1);
#else
  return 1;
#endif
}

int setFArrayThread(int n, SEXP reset_after_fork){
#ifdef _OPENMP
  if(!detectFork){
    farrayThreads = n;
  }
  if( reset_after_fork != R_NilValue ){
    if( Rf_asLogical(reset_after_fork) ){
      reset_forked = true;
    } else {
      reset_forked = false;
    }
  }

  return n;
#else
  return 1;
#endif
}


bool hasOpenMP(){
  return FARRAY_HAS_OPENMP;
}

void onForked(){
  detectFork = true;
}
void onLeaveFork(){
  if(!reset_forked){
    farrayThreads = 1;
  }
  detectFork = false;
}

int detectForked(DllInfo *dll){
  // To disable openmp if fork is detected
#ifdef _OPENMP
  return pthread_atfork(&onForked, &onLeaveFork, NULL);
#endif

  return 0;
}
