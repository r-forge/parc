#include "paRc.h"


/*
 * Matrix Multiplication
 */


void Serial_matrix_mult( double *x, int *nrx, int *ncx,
			 double *y, int *nry, int *ncy,
			 double *z) {
  int i, j, k;
  double sum;

  for(i = 0; i < *nrx; i++)
    for(j = 0; j < *ncy; j++){
      sum = 0.0;
      for(k = 0; k < *ncx; k++)
	sum += x[i + k**nrx]*y[k + j**nry];
      z[i + j**nrx] = sum;
    }
}

