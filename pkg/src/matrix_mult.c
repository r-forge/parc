#include "paRc.h"

#ifdef _OPENMP
#include <omp.h>
#endif
// the following includes R dgemm function definitions
#include <R_ext/Applic.h>

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

void OMP_matrix_mult_BLAS( double *x, int *nrx, int *ncx,
			   double *y, int *nry, int *ncy,
			   double *z) {
  int i, j, k;
  char *transa = "N", *transb = "N";
  double one = 1.0, zero = 0.0;
  int ione = 1;
  
#pragma omp parallel for shared(x, y, z, j, k, nrx, nry, ncy, ncx)
  for(i = 0; i < *nrx; i++)
    for(j = 0; j < *ncy; j++){
       F77_CALL(dgemm)(transa, transb, &ione, &ione, 
		       ncx, &one, &x[i**ncx], &ione, &y[j**nry], 
		       nry, &zero, &z[i + j**nrx], &ione);
    }
}

void OMP_matrix_mult( double *x, int *nrx, int *ncx,
		      double *y, int *nry, int *ncy,
		      double *z) {
  int i, j, k;
  double sum;

#pragma omp parallel for private(sum) shared(x, y, z, j, k, nrx, nry, ncy, ncx)
  for(i = 0; i < *nrx; i++)
    for(j = 0; j < *ncy; j++){
      sum = 0.0;
      for(k = 0; k < *ncx; k++) 
	sum += x[i + k**nrx]*y[k + j**nry];
      z[i + j**nrx] = sum;
    }
}

void matrix_mult_BLAS( double *x, int *nrx, int *ncx,
		       double *y, int *nry, int *ncy,
		       double *z) {
  int i, j, k;
  char *transa = "N", *transb = "N";
  double one = 1.0, zero = 0.0;
  int ione = 1;
  
  // #pragma omp parallel for private(sum) shared(x, y, z, j, k, nrx, nry, ncy, ncx)
  for(i = 0; i < *nrx; i++)
    for(j = 0; j < *ncy; j++){
       F77_CALL(dgemm)(transa, transb, &ione, &ione, 
		       ncx, &one, &x[i**ncx], &ione, &y[j**nry], 
		       nry, &zero, &z[i + j**nrx], &ione);
    }
}

void dot_product_BLAS( double *x, int *nx, 
		       double *y, int *ny, 
		       double *z) {
  // transx/y specifies the form of A, either N -> A or T -> A'
  char *transa = "N", *transb = "N";
  double one = 1.0, zero = 0.0;
  int ione = 1;
  F77_CALL(dgemm)(transa, transb, &ione, &ione, nx, &one,
                            x, &ione, y, ny, &zero, z, &ione);
  
  
}

