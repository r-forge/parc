/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* Collective Communication - the matrix-vector product (p. 78) */
/* theussl,2007 */
// FIXME: not working yet

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char* argv[]) {
  
  // MPI specific variables
  MPI_Status status;
  int my_rank;
  int p;

  int n = 100;

  void Get_data( float* x_ptr[], float* y_ptr[], int n );
  float Parallel_dot( float local_x[], float local_y[], int local_n);


  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &p);

  // initialize vectors
  Get_data( &x, &y, n );

  // assume that n is evenly divided by p 
  int local_n = n/p;
  float local_x[local_n];   // process local x vector
  float local_y[local_n];   // process local y vector
  
  for( i = 0; i < local_n; i++ ) {
    local_x[i] = x[i + local_n*my_rank];
    local_y[i] = y[i + local_n*my_rank];
  }

  dot = Parallel_dot(local_x, local_y, local_n)

  if(my_rank == 0){
    printf("The dot product of vectors x and y is %f\n", dot);  
  }
  // shutdown MPI
  MPI_Finalize();

  return 0;
}


/* calculate matrix-vector product 
 * serial version
 */

void Serial_matrix_vector_prod( MATRIX_T A, int m, int n, float x[], float y[] ) {
  int k, j;
  
  for( k = 0; k < m; k++ ) {
    y[k] = 0.0;
    for( j = 0; j < n; j++ )
      y[k] = y[k] + A[k][j]*x[j];
  }
}

/* calculate dot product 
 * parallel version calling MPI_Reduce
 */

float Parallel_dot( float local_x[], float local_y[], int n_bar) {
  float local_dot;
  float dot = 0.0;
  float Serial_dot( float x[], float y[], int m);

  local_dot = Serial_dot(local_x, local_y, n_bar);
  MPI_Reduce( &local_dot, &dot, 1, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
  return dot;
}

void Get_data( float* x_ptr[], float* y_ptr[], int n ) {
  if( my_rank == 0 ) {
    // random vectors
    for( i = 0; i < n; i++ ) {
      float *x[i] = random(1000)*0.01 - 5.0;
      float *y[i] = random(1000)*0.01 - 5.0;
    }
  }
  MPI_Bcast( x_ptr, n, MPI_FLOAT, 0, MPI_COMM_WORLD );
  MPI_Bcast( y_ptr, n, MPI_FLOAT, 0, MPI_COMM_WORLD );
}


