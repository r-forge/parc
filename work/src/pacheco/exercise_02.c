/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* Numerical integration - the trapezoidal rule (p. 55) */
/* theussl,2007 */

#include <stdio.h>
// #include <string.h>
#include <mpi.h>

int main(int argc, char* argv[]) {
  
  // The serial version
  /* float integral;
     float a,b;
     int n;
     float h;
     float x;
     int i;

     float f(float x);

     // No interaction on cluster
     //printf("Enter a, b and n\n");
     // scanf("%f %f %d", &a, &b, &n);

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
  */
  
  // MPI specific variables
  int source;
  int dest = 0;
  int tag = 0;
  char message[100];
  MPI_Status status;

  int my_rank;
  int p;
  float a = 1.0;   // left endpoint
  float b = 100.0; // right endpoint
  int n = 10000;
  float local_a;   // process local left endpoint
  float local_b;   // process local right endpoint
  float local_n;   // process local number of trapezoids
  float h;
  float integral;  // integral over process local interval
  float total;     // total integral
   
  float Trap( float local_a, float local_b, float local_n, float h); 

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &p);

  h = (b - a)/n;
  local_n = n/p;

  local_a = a + my_rank*local_n*h;
  local_b = local_a + local_n*h;

  integral = Trap(local_a, local_b, local_n, h);

  // process 0 in addition calculates the total integral
  if(my_rank == 0){
    total = integral;
    for(source = 1; source < p; source++){
      MPI_Recv(&integral, 1, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &status);
      total=total + integral;
    }
  }
  // the other processes send their part of the integral to process 0
  else{
    MPI_Send(&integral, 1, MPI_FLOAT, dest, tag, MPI_COMM_WORLD);
  }

  if(my_rank == 0){
    printf("With n = %d trapezoids, our estimate\n", n);  
    printf("of the integral from %f to %f = %f\n", a, b, total);
  }
  // shutdown MPI
  MPI_Finalize();

  return 0;
}

float Trap( float local_a, float local_b, float local_n, float h){
  float integral;
  float x;
  int i;

  float f(float x);

  integral = (f(local_a) + f(local_b))/2.0;
  x = local_a;
  for(i = 1; i <= local_n - 1; i++){
    x = x + h;
    integral = integral + f(x);
  }
  integral = integral*h;
  return integral;
}
  
float f(float x) {
  float return_val;
  
  return_val = x*x - 2*x + 2;
 
  return return_val;
}


