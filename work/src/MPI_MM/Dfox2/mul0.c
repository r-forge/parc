

/**************************************************************************
 * Fox's algorithm
 **************************************************************************/
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <time.h>
#include <mpi.h>
#include "mul.h"

void MULnode(  int matrix_size, int item_a[], int item_b[] ){

  int ele_a[MAX_ARRAY][MAX_ARRAY];
  int ele_b[MAX_ARRAY][MAX_ARRAY];
  int item_aa[ONED_ARRAY], item_bb[ONED_ARRAY]; 
  int diagonal[ONED_ARRAY];
  int temp_b[ONED_ARRAY];
  int temp_send[ONED_ARRAY];
  int temp_recv[ONED_ARRAY];
  int temp_result[MAX_ARRAY][MAX_ARRAY];
  int temp_a[ONED_ARRAY];
  int recv_a[ONED_ARRAY];
  int c[ONED_ARRAY];

  int oneblock[ONED_ARRAY];
  int block_a[MAX_ARRAY][MAX_ARRAY];
  int block_b[MAX_ARRAY][MAX_ARRAY];
  
  int i, j, k,l, loop_x, loop_y, loop, shift;
  
  char  name[100];     
  
  int 
    length,
    time, 
    my_node,   
    templen,
    block_size_x, block_size_y,  
    p_x, p_y, left_p, right_p,
    num_processors, num_processor_x, num_processor_y, below_processor;       

  int local_x_size,
    num_nodes,
    init_col;


  double  startTime, endTime;     
  
  
  
  MPI_Status    Status;                 
  
  struct rusage       resources;              
  
  
  MPI_Comm_rank ( MPI_COMM_WORLD, &my_node );
  MPI_Comm_size ( MPI_COMM_WORLD, &num_processors );
  gethostname ( name, 100 );
  

  if( num_processors == 1 ){
    num_processor_x = 1;
    num_processor_y = 1;
  }
  if( num_processors == 2 ){
    num_processor_x = 1;
    num_processor_y = 2;
  }
  if( num_processors == 4 ){
    num_processor_x = 2;
    num_processor_y = 2;
  }
  if( num_processors == 6 ){
    num_processor_x = 3;
    num_processor_y = 2;
  }
  if( num_processors == 8 ){
    num_processor_x = 4;
    num_processor_y = 2;
  }

  
  if( matrix_size % num_processor_x != 0 || 
      matrix_size % num_processor_y != 0 ) {
    printf( "wrong size" );
    exit( 0 );
  }
    
  block_size_x = matrix_size / num_processor_x;
  block_size_y = matrix_size / num_processor_y;


  if (my_node == HOST) {
  
 
  /* set matrix A and B */
    k=0;
    for(i=0; i<matrix_size; i++)
      for( j=0; j<matrix_size; j++){
	ele_a[i][j]=item_a[k];
	ele_b[i][j]=item_b[k];
	k++;
      }

    for(i=0; i<matrix_size; i++){
      for( j=0; j<matrix_size; j++){
	printf( " %d", ele_a[i][j]);
      }
      printf("\n");
    }
    printf("\n");
    for(i=0; i<matrix_size; i++){
      for( j=0; j<matrix_size; j++){
	printf( " %d", ele_b[i][j]);
      }
      printf("\n");
    }


    /* set matrix to scatter mode */
    k=0;
    for( loop_y = 0; loop_y<  matrix_size/block_size_y; loop_y++ ){
      for( loop_x = 0; loop_x<  matrix_size/block_size_x; loop_x++ ){
	for(i=loop_y; i<matrix_size; i += num_processor_y){
	  for( j=loop_x; j<matrix_size; j += num_processor_x){
	    item_aa[k]=ele_a[i][j];
	    item_bb[k]=ele_b[i][j];
	    
	    k++;
	  }
	}
      }
    }

    /*  printf("\n"); 
    for(i=0; i<matrix_size * matrix_size; i++){
      printf( " %d", item_aa[i]);
      if( (i+1) % ( block_size_x * block_size_y  ) == 0 )
	printf("\n");
    }
    printf("\n");

    for(i=0; i<matrix_size * matrix_size; i++){
      printf( " %d", item_bb[i]);
      if( i % 6 ==5 )
	printf("\n");
    }
    printf("\n");
    */
    
    for( i=1; i< num_processors; i++){
      MPI_Send( &item_aa[i*block_size_x * block_size_y ],
		block_size_x * block_size_y,
		MPI_INT, i, DATA_A, MPI_COMM_WORLD );
      MPI_Send( &item_bb[i*block_size_x * block_size_y ],
		block_size_x * block_size_y,
		MPI_INT, i, DATA_B, MPI_COMM_WORLD );
    }
    k=0;
    for( i = 0; i < block_size_y; i++)
      for( j = 0; j < block_size_x; j++){
	block_a[i][j] = item_aa[k];
	block_b[i][j] = item_bb[k];
	k++;
      }
  }

  /*receive data from host */
  else{
    MPI_Recv( item_aa, MAX_ARRAY, MPI_INT, HOST, DATA_A,
	      MPI_COMM_WORLD, &Status);
    MPI_Recv( item_bb, MAX_ARRAY, MPI_INT, HOST, DATA_B,
	      MPI_COMM_WORLD, &Status);
    MPI_Get_count(&Status, MPI_INT, &length);
    
    k=0;
    for( i = 0; i < block_size_y; i++)
      for( j = 0; j < block_size_x; j++){
	block_a[i][j] = item_aa[k];
	block_b[i][j] = item_bb[k];
	k++;
      }
  }  

  /* set Processor coordinator */
  if( num_processors == 1 ){
    p_x = 0;
    p_y = 0;
  }
  if( num_processors == 2 ){
    p_x = 0;
    p_y = my_node ;
  }
  if( num_processors == 4 ){
    p_x = my_node %2 ;
    p_y = (my_node  < 2 ) ? 0:1;
  }
  if( num_processors == 6){
    p_x = my_node %3 ;
    p_y = (my_node  < 3 ) ? 0:1;
  }
  if( num_processors == 8){
    p_x = my_node %4 ;
    p_y = (my_node  < 4 ) ? 0:1;
  }

  MPI_Barrier( MPI_COMM_WORLD ) ;
  
  getrusage ( RUSAGE_SELF, &resources );
  time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec;
  
  startTime = MPI_Wtime();
    
    /*      */
    printf("mynode  %d at (%d, %d)\n" , my_node, p_x, p_y); 


    for( i = 0; i < block_size_y; i++){
      for( j = 0; j < block_size_x; j++){
	printf(  " %d", block_a[i][j]);
      }
      printf("\n");
    }
    printf("\n");
    for( i = 0; i < block_size_y; i++){
      for( j = 0; j < block_size_x; j++){
	printf(  " %d", block_b[i][j]);
      }
      printf("\n");
    }
    printf("\n");
     

 

  /* Find the processor which located in the same column */
  if( my_node < num_processors / 2 )
    below_processor = my_node + num_processors/2;
  else 
    below_processor = my_node - num_processors/2;
  
  for( shift = 0; shift < matrix_size; shift++ ){
    
    /* Find the diagonal elements and send it to the below processor*/
    k=0;
    for( i = 0; i < block_size_y; i++ ){
      for( j = 0; j <block_size_x; j++ ){
	if( (j * num_processor_x + p_x + shift)%matrix_size == 
	    i * num_processor_y + p_y){
	  
	  temp_send[k] = j;
	  temp_send[k+1] = block_b[i][j];
	  k+=2;
	  /*
	    printf( " node %d ,diagonal %d at  %d, %d, \n", my_node,
		  block_b[i][j], i, j );
	  */
	  temp_b[j]=block_b[i][j];
	  /*
	    for( loop = 0; loop < block_size_y; loop++ ){
	    temp_b[loop][j] = block_b[i][j];
	    }
	  */
	}
      }
    }
    
    MPI_Send( &temp_send, 2 * block_size_x/num_processor_y, 
	      MPI_INT, below_processor, DIAGONAL, MPI_COMM_WORLD );
    
    
    MPI_Recv( temp_recv, MAX_ARRAY, MPI_INT, below_processor, DIAGONAL,
	      MPI_COMM_WORLD, &Status); 
    MPI_Get_count(&Status, MPI_INT, &length);
    /*  printf(" legnth %d, ", length ); 
	for( loop=0; loop<length; loop++ )
	printf("  %d, ", temp_recv[loop] );
    */
    for( loop = 0; loop < length; loop +=2 ){
     
	temp_b[ temp_recv[loop] ] = temp_recv[ loop+1 ];
	
    }
    
    
    /* Multiply the element of A and broadcast B */
       printf( " node %d\n", my_node );  
    for( i = 0; i < block_size_y; i++){
      for( j = 0; j < block_size_x; j++){
	temp_result[i][j] += block_a[i][j] * temp_b[j];
	
	 printf(  "%d*%d=%d\t", 
		 block_a[i][j], temp_b[j], temp_result[i][j] );
	
	
      }printf("\n");
    }printf("\n");
    
    /* Set the matrix A to 1D array and send it to the previous processor */  
    k=0;
    for( i = 0; i < block_size_y; i++){
      for( j = 0; j < block_size_x; j++){
	if ( p_x != 0                 )
	  temp_a[k] =  block_a[i][j];
	else {
	  /*printf( " %d [%d][%d] ",block_a[(i + block_size_x -1 ) % block_size_x][j],i, (j + block_size_x -1 ) % block_size_x   );
	   */
	  temp_a[k] =  block_a[ i ][(j + 1 ) % block_size_x];
	}
	k++;
      }
    }
    if( p_x == 0 )
      left_p = my_node + num_processor_x -1;
    else
      left_p = my_node -1;
    
    if( p_x == num_processor_x-1 )
      right_p = my_node - num_processor_x +1;
    else
      right_p = my_node +1;
    
    /*  printf("  %d, ", recv_a[loop] );
	printf("\n");  
	for( loop=0; loop< block_size_x* block_size_y; loop++ )
    */
    
    
    MPI_Send( &temp_a[0], block_size_x* block_size_y,
	      MPI_INT, left_p,
	      A, MPI_COMM_WORLD );
    MPI_Recv( recv_a, MAX_ARRAY, MPI_INT, right_p, A,
	      MPI_COMM_WORLD, &Status); 
    MPI_Get_count(&Status, MPI_INT, &length);
    
    k=0;
    for( i = 0; i < block_size_y; i++)
      for( j = 0; j < block_size_x; j++){
	block_a[i][j] = recv_a[k];
	/*printf("  %d ", block_a[i][j] ); */
	
	k++;
      }
    
  }


  /* set result array to OneD array before send */
  k=0;
  for( i = 0; i < block_size_y; i++){
    for( j = 0; j < block_size_x; j++){    
      temp_send[k] = temp_result[i][j];
      k++;
    }
  }


  /* Send result */
  if( my_node != HOST ){
    MPI_Send( &temp_send[0], block_size_y * block_size_x, 
	      MPI_INT, HOST,
	      C, MPI_COMM_WORLD );
  }

  else{
    l=0;
    for( k=0; k< length; k++){
      if( l % matrix_size  != 0 ){
	c[l] = temp_send[k];
	l += num_processor_x;
      }
      else{
	if ( num_processor_y == 2 && l !=0 ) {
	  l += matrix_size ;
	}
	c[l] = temp_send[k];
	l += num_processor_x;
      }
    }
    printf( "result at %d\n", my_node);
    for( k = 0; k < matrix_size * matrix_size; k++){
      if( k % matrix_size == 0 )
	printf( "\n");
      printf( " %d", c[k] );
      
    }	  printf( "\n");
    
    
    
  
    
    /* receive the result from the processor */
    for( loop = 1; loop < num_processors; loop++ ){
      MPI_Recv( temp_recv, MAX_ARRAY, MPI_INT,
		loop, C,
		MPI_COMM_WORLD, &Status );
      MPI_Get_count( &Status, MPI_INT, &templen );
      if( loop < num_processors /2 ){
	l=loop;
	for( k=0; k< templen; k++){
	  c[l] = temp_recv[k];
	  l += num_processor_x;
	  if( (k+1) % block_size_x == 0 )
	    l += matrix_size ;
	  
	}  
      }
      else{
	l = loop - num_processor_x + matrix_size;
	for( k=0; k< templen; k++){
	  c[l] = temp_recv[k];
	  l += num_processor_x;
	  if( (k+1) % block_size_x == 0 )
	    l += matrix_size ;
	  
	}  
      }
      printf( "result at %d\n", loop);
      for( k = 0; k < matrix_size * matrix_size; k++){
	if( k % matrix_size == 0 )
	  printf( "\n");
	printf( " %d", c[k] );
	
      }	  printf( "\n");
      
    }
    
 
 
  
  
 
    
    
  printf("\n");
  
  getrusage(RUSAGE_SELF, &resources);
  time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec 
    - time;
  
  endTime = MPI_Wtime() - startTime;
  

 printf( "result aa\n");
  for( k = 0; k < matrix_size * matrix_size; k++){
    if( k % matrix_size == 0 )
      printf( "\n");
    printf( " %d", c[k] );
    
  }	  printf( "\n");
  
  
 
    

  
  printf("\n\ntime=%6.6f seconds/%6.6f seconds\n\n",
	 (float)time/1000000.0, endTime);
  
  fflush(stdout);
  

  }
  MPI_Barrier(MPI_COMM_WORLD);
}
  
  void main(int argc,char *argv[])
{
  
  int matrix_size, 
    num_processors, i, 
    item_a[ ONED_ARRAY ], 
    item_b[ ONED_ARRAY ];
  
  /*  Set up mpi machine */
  MPI_Init(&argc, &argv);
  
  if ( argc != 1 ){
	printf("Usage:  %s  < inputfile\n", *argv);
	exit(1);
  }
  
  scanf( "%d", &matrix_size );
  /*printf( " %d", matrix_size*matrix_size );
  if (matrix_size % 12  != 0 ){
    printf("Matrix size should be the multiple of 12\n" );
    exit(1);
  }*/
  /* scan matrix A */
  for ( i = 0; i < matrix_size * matrix_size ; ++i ){
	scanf ( "%d", &item_a [ i ]);
  }

  /*  printf( " \n%d\n", matrix_size );
  for ( i = 0; i < matrix_size * matrix_size ; ++i ){
    printf( " %d", item_a[i] );
    
  }
  */
  /* scan matrix B */
  for ( i = 0; i < matrix_size * matrix_size ; ++i ){
	scanf ( "%d", &item_b [ i ]);
  }
  
  /*
 for ( i = 0; i < matrix_size * matrix_size ; ++i ){
    printf( " %d", item_a[i] );
 
  }
  printf( "\n");
  for ( i = 0; i < matrix_size * matrix_size ; ++i ){
    printf( "  %d", item_b[i] );
  }
  */
MULnode( matrix_size, item_a, item_b );  

/*  


printf( "before print");
  for ( i = 0; i < matrix_size*matrix_size ; ++i ){
    if( i % matrix_size == 0 )
      printf( "\n");
    printf("ele[%d]: %d\t", i, item[i] );
    printf( "   1  ");
  }
  printf( "end main");


  */  
  MPI_Finalize();
}









