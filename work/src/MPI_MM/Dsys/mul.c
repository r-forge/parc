/**************************************************************************
 * Systolic algorithm using dynamic allocation.
 **************************************************************************/
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <time.h>
#include <mpi.h>
#include "mul.h"

void MULnode( int argc, char *argv[] ){
  int 
    *ele_a,
    *ele_b,
    *temp,
    *result,
    *c;

  char  name[100];     

  int 
    i,j,k,l,
    matrix_size,
    local_x_size,
    length,  templen,
    time,
    my_node, loop,   
    num_nodes,
    init_col, start_point,
    block_size,
    num_processors;       
  double  startTime, endTime;     

  MPI_Status
	Status;                 
  
  struct rusage   
	resources;              


  MPI_Comm_rank ( MPI_COMM_WORLD, &my_node );
  MPI_Comm_size ( MPI_COMM_WORLD, &num_processors );
  gethostname ( name, 100 );
  
  scanf("%d", &matrix_size);
  
  if( matrix_size <2 || matrix_size > MAX_ONED_ARRAY ){
    /*      || matrix_size %12 != 0){*/
    if( my_node == HOST ){
      printf( "The matrix size must be between 2 and %d\n, and the matrix size must be the multiple of 12\n", MAX_ONED_ARRAY ); 
      fflush(stdout);
    }
    
    exit(0);
  }
  if( num_processors != 1 && num_processors != 4 &&
      num_processors != 6 && num_processors != 12 ){
    if( my_node == HOST )
      printf( "The processors must be 1, 4, 6, or 12 \n");
    exit(0);
  }
  else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ) ) == 0 ||
	   (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ) ) == 0 ||
	   (temp = malloc( sizeof(int) * matrix_size * matrix_size ) ) == 0 ||
	   (result= malloc( sizeof(int) * matrix_size * matrix_size ) ) == 0 ||
	   (c = malloc( sizeof(int) * matrix_size * matrix_size ) ) == 0 ){
    if( my_node == HOST ){
      printf( stderr, "Unable to allocate enough memory, exiting...\n");
    }
    exit(0);   
  }

  MPI_Barrier( MPI_COMM_WORLD ) ;
  getrusage ( RUSAGE_SELF, &resources );
  time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec;
  
  startTime = MPI_Wtime();
  
  block_size = matrix_size / num_processors;      
  init_col = block_size * my_node;
  start_point = my_node*block_size;  
  length = matrix_size * block_size;

  if( my_node == HOST ) {
    /*generate the matrices A and B */
    for( i =0; i < matrix_size* matrix_size; i++){
      ele_a[i] = rand()%10;
      ele_b[i] = rand()%10;
    }
    /*Transpose matrix B */
    for( i =0; i < matrix_size; i++){
      for( j =0; j < matrix_size; j++){
	temp[j*matrix_size + i] = ele_b[i*matrix_size +j];
      }
    }
    for( i =0; i < matrix_size; i++){
      for( j =0; j < matrix_size; j++){
	ele_b[i*matrix_size + j] = temp[i*matrix_size +j];
      }
    }
    /* Print the matrix A and transposed B 
    for( i =0; i < matrix_size* matrix_size; i++){
      printf(" %d", ele_a[i]);
      if( i % matrix_size == matrix_size -1 )
	printf( "\n");
    }  printf( "\n");
    for( i =0; i < matrix_size* matrix_size; i++){
      printf(" %d", ele_b[i]);
      if( i % matrix_size == matrix_size-1 )
	printf( "\n");
    }    
    */
    /* Send Submatrices to the processors*/
    for( i=1; i< num_processors; i++){
      MPI_Send( &ele_a[i*(matrix_size*block_size) ], 
		matrix_size * block_size, 
		MPI_INT, i, A, MPI_COMM_WORLD );
      MPI_Send( &ele_b[i*(matrix_size*block_size) ], 
		matrix_size * block_size, 
		MPI_INT, i, B, MPI_COMM_WORLD );    
    }
  }
  /* Other processors receive the submatrices */
  else{
    MPI_Recv( ele_a, MAX_ONED_ARRAY, MPI_INT, HOST, A,
	      MPI_COMM_WORLD, &Status); 
    MPI_Recv( ele_b, MAX_ONED_ARRAY, MPI_INT, HOST, B,
	      MPI_COMM_WORLD, &Status); 
    MPI_Get_count( &Status, MPI_INT, &length);
  }
  /* initialize the result matrix */
  for( i=0; i < length; i++){
    result[i]=0;
  }

  /* multiplication */
  for( loop = 0; loop<num_processors; loop++ ){
    for(i=0; i<block_size; i++){
      for( j=0; j< block_size; j++ ){
	for( k=0; k< matrix_size; k++){
	  
	  result[i * matrix_size + (start_point+j) % matrix_size] 
	    += ele_a[i* matrix_size+ k] * ele_b[j*matrix_size+k];
	} 
      }
    } 
    /* upward circular shift B as much as one block */
    
    MPI_Send( &ele_b[0],matrix_size * block_size,
	      MPI_INT,  (my_node-1+num_processors) % num_processors ,
	      AFTER, MPI_COMM_WORLD );
    MPI_Recv( ele_b, MAX_ONED_ARRAY, MPI_INT, 
	      (my_node+1) % num_processors, AFTER,
	      MPI_COMM_WORLD, &Status);     
    
    start_point += block_size;
    
  }
     
  /* send result to host */
  if( my_node != HOST )
    MPI_Send( &result[0], block_size * matrix_size,
	      MPI_INT,  HOST,
	      RESULT, MPI_COMM_WORLD );
  /* Host processor receives the results and generate the matrux C */
  else{
    l=0;
    for( i=0; i < block_size; i++)
      for( j=0; j < matrix_size; j++){
	c[i*matrix_size + j] = result[l];
	l++;
      }
    
    for ( i=1; i< num_processors; i++ ){
      MPI_Recv( result, MAX_ONED_ARRAY, MPI_INT, 
		i, RESULT,
		MPI_COMM_WORLD, &Status);     
     
      l=0;
      for( j=i*block_size; j<(i+1)*block_size; j++)
	for ( k=0; k< matrix_size; k++){
	  c[j* matrix_size + k] = result[l];
	  l++;
	}
      
    }
    
    getrusage(RUSAGE_SELF, &resources);
    time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec 
      - time;
    
    endTime = MPI_Wtime() - startTime;
    /*
    printf("\n");
    for(i=0; i<matrix_size* matrix_size; i++){
      printf( "%d ", c[i]);
      if( i%matrix_size == matrix_size-1 )
	    printf("\n");
    }    
    */
    printf("time=%6.6f seconds/%6.6f seconds\n\n",
	   (float)time/1000000.0, endTime);
    fflush(stdout);
  }  
  
  MPI_Barrier(MPI_COMM_WORLD);
  
  free(ele_a);
  free(ele_b);
  free(temp);
  free(result);
  free(c);
  
  
}
  
  void main(int argc,char *argv[])
{
  
  
  /*  Set up mpi machine */
  MPI_Init(&argc, &argv);

  MULnode( argc, argv );
  
  MPI_Finalize();
}









