/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* Numerical integration; Collective Communication (p. 66) */
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

// Calculates the ceiling of log_2(x)
int Ceiling_log2( int x) {
  unsigned temp = (unsigned) x - 1;
  int result = 0;

  while (temp != 0) {
    temp = temp >> 1;
    result = result + 1;
  }

  return result;
}

// calculate wheter a process receives and, if so, the source
int I_receive( int stage, int my_rank, int* source_ptr ) {
  int power_2_stage;
  power_2_stage = 1 << stage; // 2^stage = 1 << stage
  if( ( power_2_stage <= my_rank ) && ( my_rank < 2*power_2_stage ) ) {
    *source_ptr = my_rank - power_2_stage;
    return 1;
  }
  else return 0;
}

// calculate wheter a process sends and, if so, the destination
int I_send( int stage, int my_rank, int p, int* dest_ptr ) {
  int power_2_stage;
  power_2_stage = 1 << stage; // 2^stage = 1 << stage
  if( my_rank < power_2_stage ) {
    *dest_ptr = my_rank + power_2_stage;
    if( *dest_ptr >= p) return 0;
    else return 1;
  }
  else return 0;
}

void Send( float a, float b, int n, int dest ) {
  MPI_Send( &a, 1, MPI_FLOAT, dest, 0, MPI_COMM_WORLD);
  MPI_Send( &b, 1, MPI_FLOAT, dest, 1, MPI_COMM_WORLD);
  MPI_Send( &n, 1, MPI_INT, dest, 2, MPI_COMM_WORLD);
}

void Receive( float* a_ptr, float* b_ptr, int* n_ptr, int source ) {
  MPI_Status status;

  MPI_Recv( a_ptr, 1, MPI_FLOAT, source, 0, MPI_COMM_WORLD, &status);
  MPI_Recv( b_ptr, 1, MPI_FLOAT, source, 1, MPI_COMM_WORLD, &status);
  MPI_Recv( n_ptr, 1, MPI_INT, source, 2, MPI_COMM_WORLD, &status);
}

/* Get-Data
 * normally reads in user input, but on cluster this is not possible
 * 
 * Alternative 1: linear I/O see exercise_02-2.c
 * Alternative 2: tree structured communication see beneath
 * Alternative 3: broadcasts see Get-data2
 *
 * FIXME: reads values and function to integrate from a file
 */

void Get_data( float* a_ptr, float* b_ptr, int* n_ptr, int my_rank,
	       int p ) {
  int source;
  int dest;
  int stage;

  if( my_rank == 0 ) {
  // printf("Enter a, b and n\n");
  // scanf("%f %f %d", a_ptr, b_ptr, n_ptr);
  *a_ptr = 1.0;
  *b_ptr = 100.0;
  *n_ptr = 10000;
  }
   
  for( stage = 0; stage < Ceiling_log2(p); stage++ ) {
    if( I_receive( stage, my_rank, &source ) )
      Receive( a_ptr, b_ptr, n_ptr, source );
    else if( I_send( stage, my_rank, p, &dest) )
      Send( *a_ptr, *b_ptr, *n_ptr, dest);
  }

}

void Get_data2( float* a_ptr, float* b_ptr, int* n_ptr, int my_rank ) {
  if( my_rank == 0 ) {
    // printf("Enter a, b and n\n");
    // scanf("%f %f %d", a_ptr, b_ptr, n_ptr);
    *a_ptr = 1.0;
    *b_ptr = 100.0;
    *n_ptr = 10000;
  }
  MPI_Bcast( a_ptr, 1, MPI_FLOAT, 0, MPI_COMM_WORLD );
  MPI_Bcast( b_ptr, 1, MPI_FLOAT, 0, MPI_COMM_WORLD );
  MPI_Bcast( n_ptr, 1, MPI_FLOAT, 0, MPI_COMM_WORLD );
}

