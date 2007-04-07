/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* Numerical integration with I/O - the trapezoidal rule (p. 61) */
/* theussl,2007 */

#include <stdio.h>
// #include <string.h>
#include <mpi.h>

int main(int argc, char* argv[]) {
  
  // MPI specific variables
  int source;
  int dest = 0;
  int tag = 0;
  char message[100];
  MPI_Status status;

  int my_rank;
  int p;
  float a;   // left endpoint
  float b; // right endpoint
  int n;
  float local_a;   // process local left endpoint
  float local_b;   // process local right endpoint
  float local_n;   // process local number of trapezoids
  float h;
  float integral;  // integral over process local interval
  float total;     // total integral
   
  float Trap( float local_a, float local_b, float local_n, float h); 
  void Get_data( float* a_ptr, float* b_ptr, int* n_ptr, int my_rank,
		 int p);

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &p);

  Get_data( &a, &b, &n, my_rank, p);

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

/* Get-data
 * normally reads in user input, but on cluster this is not possible
 * 
 * Alternative 1: linear I/O see below
 * Alternative 2: tree structured communication see exercise_02-3.c
 * Alternative 3: broadcasts see Get-data2 see exercise_02-3.c
 *
 * FIXME: reads values and function to integrate from a file
 */

void Get_data( float* a_ptr, float* b_ptr, int* n_ptr, int my_rank,
	       int p) {
  int source = 0;
  int dest;
  int tag;
  MPI_Status status;
 

  if( my_rank == 0 ) {
    // printf("Enter a, b and n\n");
    // scanf("%f %f %d", a_ptr, b_ptr, n_ptr);
    *a_ptr = 1.0;
    *b_ptr = 100.0;
    *n_ptr = 10000;
    for( dest = 1; dest < p; dest++ ) {
      tag = 0;
      MPI_Send( a_ptr, 1, MPI_FLOAT, dest, tag, MPI_COMM_WORLD);
      tag = 1;
      MPI_Send( b_ptr, 1, MPI_FLOAT, dest, tag, MPI_COMM_WORLD);
      tag = 2;
      MPI_Send( n_ptr, 1, MPI_INT, dest, tag, MPI_COMM_WORLD);
    }
  }
  else {
    tag = 0;
    MPI_RECV( a_ptr, 1, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &status);
    tag = 1;
    MPI_RECV( b_ptr, 1, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &status);
    tag = 2;
    MPI_RECV( n_ptr, 1, MPI_INT, source, tag, MPI_COMM_WORLD, &status);
  }
}



