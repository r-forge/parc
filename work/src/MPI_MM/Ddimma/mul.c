
/**************************************************************************
 * DIMMA algorithm using two dimensional processor template.
 **************************************************************************/
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <time.h>
#include <mpi.h>
#include "mul.h"

void MULnode(  int argc, char *argv[] ){

  int 
    *ele_a,
    *ele_b,
    *block_a,
    *block_b,
    *temp_result,
    *c;

  
  int i, j, k,l, loop,   loop_x, loop_y, shift;
  
  char  name[100];     
  
  int 
    matrix_size,
    lcm,
    block_size_x, block_size_y,  
    num_processors, num_processor_x, num_processor_y,
    x, y,  /* the start point of result matrix of each processor */
    time, 
    my_node; 

  double  startTime, endTime;     
  
  MPI_Status    Status;                 
  
  struct rusage       resources;              
  
  
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
  if( num_processors == 2 ||
      num_processors == 3 ||
      num_processors == 5 ||
      num_processors == 7 ||
      num_processors == 8 ||
      num_processors == 10 ||
      num_processors == 11 ||
      num_processors == 13 ||
      num_processors == 14 ||
      num_processors == 15 ){
    printf("The number of processor should be 1, 4, 6, 9, 12, or 16\n");
    exit(0);
  }
 else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	  (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	  (block_a=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	  (block_b=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	  (temp_result= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	  (c= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ){	
   if( my_node == HOST ){
      printf( stderr, "Unable to allocate enough memory, exiting...\n");
    }
    exit(0);   
  }


  if( num_processors == 1 ){
    num_processor_x = 1;
    num_processor_y = 1;
    lcm=1;
  }
  if( num_processors == 4 ){
    num_processor_x = 2;
    num_processor_y = 2;
    lcm=2;
  }
  if( num_processors == 6 ){
    num_processor_x = 3;
    num_processor_y = 2;
    lcm=6;
  }
  if( num_processors == 9 ){
    num_processor_x = 3;
    num_processor_y = 3;
    lcm=3;
  }
  if( num_processors == 12 ){
    num_processor_x = 4;
    num_processor_y = 3;
    lcm=12;
  }
  if( num_processors == 16 ){
    num_processor_x = 4;
    num_processor_y = 4;
    lcm=4;
  }
  
      
  block_size_x = matrix_size / num_processor_x;
  block_size_y = matrix_size / num_processor_y;
  MPI_Barrier( MPI_COMM_WORLD ) ;
  
  getrusage ( RUSAGE_SELF, &resources );
  time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec;
  
  startTime = MPI_Wtime();

  if (my_node == HOST) {
   /*generate the matrices A and B */
    for( i =0; i < matrix_size* matrix_size; i++){
      ele_a[i] = rand()%10;
      ele_b[i] = rand()%10;
    }

    for( i=0; i < matrix_size; i++){
      for( j=0; j < matrix_size; j++){
	printf( " %d", ele_a[i*matrix_size + j]);
      }
      printf("\n");
    }   printf("\n");
    for( i=0; i < matrix_size; i++){
      for( j=0; j < matrix_size; j++){
	printf( " %d", ele_b[i*matrix_size + j]);
      }
      printf("\n");
    }   printf("\n");

  }
 /* initialize the result matrix */
  for( i=0; i < block_size_x*block_size_y; i++){
    temp_result[i]=0;
  }   
  for( loop = 0; loop< lcm; loop++){
    if( my_node == HOST ){
      
      /* set a column A and a row B */
      k=0;
      for( l=0; l< num_processor_y; l++)
	for( j=0; j< matrix_size; j+= lcm)
	  for( i = l*matrix_size/num_processor_y; 
	       i < (l+1)*matrix_size/num_processor_y; i++){
	    block_a[k] = ele_a[i*matrix_size + loop+j];
	    k++;
	  }
      k=0;
      for( l=0; l< num_processor_x; l++)
	for( j=0; j< matrix_size; j+= lcm)
	  for( i = l*matrix_size/num_processor_x;
	       i <(l+1)* matrix_size/num_processor_x; i++){
	    block_b[k] = ele_b[(loop+j)*matrix_size+i];
	    k++;
	  }
      /*
      for( i=0; i< matrix_size * matrix_size/lcm; i++)
	printf(" %d", block_a[i] );
      for( i=0; i< matrix_size * matrix_size/lcm; i++)
	printf(" %d", block_b[i] );
      */
      /* broadcast A and B */
      for( j=0; j < num_processor_y; j++){
	for( i=j*num_processor_x; i< (j+1)*num_processor_x; i ++){
	  if( i != 0 ){
	    MPI_Send( &block_a[j* block_size_y*matrix_size/lcm],
		      block_size_y*matrix_size/lcm,
		      MPI_INT, i, DATA_A, MPI_COMM_WORLD );
		    
	  }
	}
      }
      for( j=0; j < num_processor_x; j++){
	for( i=j; i< num_processors; i+= num_processor_x){
	  MPI_Send( &block_b[j* block_size_x*matrix_size/lcm],
		    block_size_x*matrix_size/lcm,
		    MPI_INT, i, DATA_B, MPI_COMM_WORLD );
	  /* printf( "send B to :%d from %d\n", i, block_b[j* block_size_x] );
	   */
	}
      }            
    }
  
    /* Other processor receive data from host */
    else{
      MPI_Recv( block_a, MAX_ONED_ARRAY, MPI_INT, HOST, DATA_A,
		MPI_COMM_WORLD, &Status);
      MPI_Recv( block_b, MAX_ONED_ARRAY, MPI_INT, HOST, DATA_B,
		MPI_COMM_WORLD, &Status);
    }
    /*
      printf( "node( %d): ", my_node );*/
    if( my_node == 0 ){
      for( i=0; i < block_size_y*matrix_size/lcm; i++)
	printf( " %d", block_a[i] );
      printf("\t");
      for( i=0; i < block_size_x * matrix_size/lcm; i++)
	printf( " %d", block_b[i] );
      printf("\n");
    }

    
    /* multiply A and B */
    for( i = 0; i < block_size_y; i++)
      for( j = 0; j < block_size_x; j++ )
	for( k = 0; k < matrix_size/lcm; k++){ 
	temp_result[i*block_size_x+j] += 
	  block_a[k*block_size_y +i] * block_b[k*block_size_x+j];
	/*  printf( "node( %d): ", my_node );
	  if(my_node == 0 )
	    printf( " %d*%d=%d(i:%d,j:%d,k:%d)", 
		    block_a[k*block_size_x +i] ,
		    block_b[k*block_size_x+j],
		    temp_result[i*block_size_x+j],
		    i,j,k); 
	*/
	} 
  }
  /*
  printf( "node( %d): ", my_node );
  for( i=0; i< block_size_y*block_size_x; i++)
    printf( "  %d", temp_result[i]);
  */
  /* Send result  */
  if( my_node != HOST ){
    MPI_Send( &temp_result[0], block_size_y * block_size_x, 
	      MPI_INT, HOST,
	      C, MPI_COMM_WORLD );
  }
  
  else{
    /* Set host's result */
    k=0; 
    for( i=0; i < block_size_y; i++)
      for ( j=0; j< block_size_x; j++){
	c[i*matrix_size+j] = temp_result[k];
	k++;
      }
    /* receive the result from the processor  */
    for( loop = 1; loop < num_processors; loop++ ){
      MPI_Recv( temp_result, MAX_ONED_ARRAY, MPI_INT,
		loop, C,
		MPI_COMM_WORLD, &Status );
      x = loop % num_processor_x * block_size_x;
      y = loop / num_processor_x * block_size_y;
      k=0;
      for( i = 0; i < block_size_y; i++)
	for( j =0; j < block_size_x; j++){
	  c[y * matrix_size + i*matrix_size+j+x] = temp_result[k];
	  k++;
	}
    }
    
    
    getrusage(RUSAGE_SELF, &resources);
    time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec 
      - time;
    
    endTime = MPI_Wtime() - startTime;
    
    
    printf( "result aa\n");
    for( i = 0; i < matrix_size; i++){
      for( j = 0; j < matrix_size; j++)
	printf( " %d", c[i* matrix_size+j] );
   
      printf( "\n");
      
    }	  printf( "\n");
    
    printf("\n\ntime=%6.6f seconds/%6.6f seconds\n\n",
	   (float)time/1000000.0, endTime);
    
    fflush(stdout);
    
    
  } 
  MPI_Barrier(MPI_COMM_WORLD);
  free( ele_a );
  free( ele_b );
  free( block_a );
  free( block_b );
  free( temp_result );
  free( c );



}

  void main(int argc,char *argv[])
{
    
  /*  Set up mpi machine */
  MPI_Init(&argc, &argv);
   
MULnode( argc, argv );

  MPI_Finalize();
}









