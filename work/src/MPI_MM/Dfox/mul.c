/**************************************************************************
 * Fox's algorithm using square processor template.
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
    *result,
    *temp_result,
    *temp_send_a,
    *temp_send_b,
    *temp_recv_a,
    *temp_recv_b,
    *block_a,
    *block_b,
    *temp_a,
    *temp_b;
  int i, j, k,l,x,y, loop;
  
  char  name[100];     

  int 
    matrix_size,
    num_processors, num_processor_x,
    block_size,
    start_x, start_y,
    to_a, from_a, to_b, from_b,
    time,
    my_node;
  
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
      num_processors != 9 && num_processors != 16 ){
    if( my_node == HOST )
      printf( "The processors must be 1, 4, 9, or 16 \n");
    exit(0);
  }
  else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (result= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_result= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (temp_send_a= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (temp_send_b= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (temp_recv_a= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (temp_recv_b= malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (block_a=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (block_b=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (temp_a= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_b= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ){
    if( my_node == HOST ){
      printf( stderr, "Unable to allocate enough memory, exiting...\n");
    }
    exit(0);   
  }
  
  
  if( num_processors == 1 )
    num_processor_x = 1;
  else if( num_processors == 4 )
    num_processor_x = 2;
  else if( num_processors == 9 )
    num_processor_x = 3;
  else if( num_processors == 16 )
    num_processor_x = 4;
 
  block_size = matrix_size / num_processor_x;

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

    /* Set host's block */
    for(i=0; i<block_size; i++)
      for(j=0; j<block_size; j++){
	block_b[i*block_size+j] = ele_b[i*matrix_size+j];
      }

   
    /* send each block to each processor */
    loop =0;
    for( y = 0; y< num_processor_x; y++ ){
      for( x = 0; x< num_processor_x; x++){
	if( loop != 0 ){
	  k=0;	
	  for( i=0; i< block_size; i++){
	    for( j=0; j< block_size; j++){
	      temp_send_b[k] = 
		ele_b[ (y*block_size + i)* matrix_size +(x* block_size +j)];
	      k++;
	    }
	  }
	  
	  MPI_Send( &temp_send_b[0],
		    block_size * block_size, 
		    MPI_INT, loop, DATA_B, MPI_COMM_WORLD );

	}
	loop++;
      }
    }
  }

  else{
    
    /*receive  B block form host */
    MPI_Recv( block_b, MAX_ONED_ARRAY, MPI_INT, HOST, DATA_B,
	      MPI_COMM_WORLD, &Status); 
  }
  /* initialize the result matrix */
  for( i=0; i < block_size*block_size; i++){
    result[i]=0;
  }  
  
  /* multiply */
  for( loop = 0; loop< matrix_size; loop++ ){
    /* Host processor sends the diagonal of A */
    if( my_node == HOST ){
      /* set an array with diagonal elements */
      for( i=0; i< matrix_size; i++)
	temp_send_a[i] = ele_a[i* matrix_size + ((i+loop)%matrix_size )];
      /* send the diagonal to other processors */
      for( j = 0; j< num_processor_x; j++)
	for( l=j*num_processor_x; l<j*num_processor_x + num_processor_x;l++){ 
	  if( l == 0 )
	    for ( i=0; i< block_size; i++)
	      block_a[i] = temp_send_a[i];	  
	  else
	    MPI_Send( &temp_send_a[j*block_size],block_size,
		      MPI_INT, l, DATA_A, MPI_COMM_WORLD );
	}
      
    }
    /* processors receive the diagonal elements of A */
    else{
      MPI_Recv( block_a, MAX_ONED_ARRAY, MPI_INT, HOST, DATA_A,
		MPI_COMM_WORLD, &Status);
    }
      /* for( i=0; i< block_size; i++)
      for( j=0; j< block_size; j++)
	printf( " %d", block_a[i][j] );
    printf("\n");
    for( i=0; i< block_size; i++)
      for( j=0; j< block_size; j++)
	printf( " %d", block_b[i][j] );
    printf("\n");
    printf(" %d\n", my_node);
      */
    for(i=0; i<block_size; i++){
      for(j=0; j<block_size; j++){
	result[i*block_size+j]+=block_a[i] * block_b[i*block_size+j];
	/*printf( " %d*%d=%d", block_a[i], block_b[i][j], result[i][j] ); */
      }
    }

    /* left-shift A and upward shift B */
    to_b = (my_node < num_processor_x )?
      my_node + num_processor_x*(num_processor_x -1) :
      my_node - num_processor_x;
    from_b = (my_node >= num_processors - num_processor_x )?
      my_node % num_processor_x :
      my_node + num_processor_x;

    /* set temporary array to send data */
    /* for( i=0; i< block_size; i++){
      temp_send_b[i] = block_b[i];
      }*/
    MPI_Send( &block_b[0], block_size,
	      MPI_INT, to_b,
	      DATA_B, MPI_COMM_WORLD );
    MPI_Recv( temp_recv_b, MAX_ONED_ARRAY, MPI_INT, 
	      from_b, DATA_B,
	      MPI_COMM_WORLD, &Status);     
    /*set new block with received array */
    /*  printf("recv ");
    for( l=0; l< block_size; l++)
      printf( " %d", temp_recv_a[l]);
      printf("\n"); */
    
    for( i=0; i< block_size-1; i++){
      for( j=0; j< block_size; j++)
	temp_b[i*block_size + j]= block_b[(i+1) * block_size +j];
    }
    for( k=0; k< block_size; k++)
      temp_b[i* block_size +k] = temp_recv_b[k];
    for( i=0; i< block_size; i++)
      for( j=0; j< block_size; j++){
	block_b[i*block_size +j] = temp_b[i*block_size +j];
      }
   

  }
  
  
 
  /* send result to host */
  if( my_node != HOST ){
    /* k=0;
    for( i=0; i< block_size; i++)
      for( j=0; j< block_size; j++){
	temp_result[k] = result[i][j];
	k++;
      }
    */
    MPI_Send( &result[0], block_size * block_size,
	      MPI_INT,  HOST,
	      RESULT, MPI_COMM_WORLD );
  }
  /*receive the result */
  else{
    for(i =0; i < block_size* block_size; i++){
      temp_result[i] = result[i];
    }
    k=0;
    for(i =0; i < block_size; i++)
      for(j =0; j < block_size; j++){
	result[i*matrix_size + j] = temp_result[k];
	k++;
      }
    

    for ( loop=1; loop< num_processors; loop++ ){
      MPI_Recv( temp_result, MAX_ONED_ARRAY, MPI_INT, 
		loop, RESULT,
		MPI_COMM_WORLD, &Status);     
      start_x = (loop % num_processor_x) * block_size;
      start_y = (loop / num_processor_x) * block_size;
      k=0;
      for( i=0; i< block_size; i++)
	for( j=0; j< block_size; j++){
	  result[(start_y +i) * matrix_size + (start_x +j)] = temp_result[k];
	  k++;
	}
    }


    
    getrusage(RUSAGE_SELF, &resources);
    time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec 
      - time;
    
    endTime = MPI_Wtime() - startTime;

   
    printf("\n");
    for(i=0; i<matrix_size; i++){
      for(j=0; j<matrix_size; j++){
	printf( "%d ", result[i*matrix_size+j]);
      }    
      printf("\n");
    }
   
    
    printf("time=%6.6f seconds/%6.6f seconds\n\n",
	   (float)time/1000000.0, endTime);
    fflush(stdout);
  }
  
  MPI_Barrier(MPI_COMM_WORLD);
  free(ele_a);
  free(ele_b);
  free(result);
  free(temp_result);
  free(temp_send_a);
  free(temp_send_b);
  free(temp_recv_a);
  free(temp_recv_b);
  free(block_a);
  free(block_b);
  free(temp_a);
  free(temp_b);
       
  
}
  
void main(int argc, char *argv[])
{
  /*  Set up mpi machine */
  MPI_Init(&argc, &argv);
  
  MULnode( argc, argv );
  
  MPI_Finalize();
}









