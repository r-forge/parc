/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* Numerical integration - the trapezoidal rule (p. 55) */
/* theussl,2007 */

#include <stdio.h>
// #include <string.h>
// #include <mpi.h>

int main(int argc, char* argv[]) {
  // The serial version
  float integral;
  float a,b;
  int n;
  float h;
  float x;
  int i;

  float f(float x);

  /* No interaction on cluster
  printf("Enter a, b and n\n");
  scanf("%f %f %d", &a, &b, &n);
  */
  a = 1.0;
  b = 100.0;
  n = 1000000;

  h = (b - a)/n;
  integral = (f(a) + f(b))/2.0;
  x = a;
  for (i = 1; i <= n-1; i++) {
    x = x + h;
    integral = integral + f(x);
  }
  integral = integral* h;

  printf("With n = %d trapezoids, our estimate\n", n);
  printf("of the integral from %f to %f = %f\n", a, b, integral);
  
  return 0;
}

  /*  
  int my_rank;
  int p;
  int source;
  int dest;
  int tag=0;
  char message[100];
  MPI_Status status;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &p);



  MPI_Finalize();
  */

float f(float x) {
  float return_val;
  
  return_val = x*x - 2*x + 2;
 
  return return_val;
}


