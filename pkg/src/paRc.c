#include "paRc.h"

#ifdef _OPENMP
#include <omp.h>
#endif

/*
 * Open Test Function
 */


void OMP_test() {
#pragma omp parallel
  {
    Rprintf("this is thread %d of %d",omp_get_thread_num(),
    omp_get_num_threads());
  }
}
