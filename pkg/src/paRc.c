#include "paRc.h"

#ifdef _OPENMP
#include <omp.h>
#endif

/*
 * OpenMP function wrappers
 */

void OMP_get_num_procs(int *n) {
  *n = omp_get_num_procs();
}

void OMP_set_num_threads(int *n) {
  int count = *n;
  omp_set_num_threads(count);
}

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


void serial_calc_pi(int *n, double *pi) {
  double w = 1.0/(double) *n;
  double sum = 0, x, f_x;
  int i;
  for (i = 1; i <= *n; i++) {
    x = w*((double)i - 0.5);
    f_x = 4/(1 + x*x);
    sum += f_x;
  }
  *pi = w*sum;
}

// c't OpenMP example: calculate PI
void OMP_calc_pi(int *n, double *pi) {
  int i;
  double w, sum, x;

  w = 1.0/(double) *n;
  sum = 0;
#pragma omp parallel for private(x) shared(w) reduction(+:sum)
  for (i = 1; i <= *n; i++) {
    x = w*((double)i - 0.5);
    sum += 4.0/(1.0 + x*x);
  }
  *pi = w*sum;
}
