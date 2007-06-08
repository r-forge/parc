#include "paRc.h"


// calculates the serial dot product
void Serial_dot( double *x, double *y, Sint *n, double *prod ) {
  int i;
  double sum = 0.0;

  for( i = 0; i < *n; i++ )
    sum = sum + x[i] * y[i];
  *prod = sum;
}



