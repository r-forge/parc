/********************************************************
 *  pumma2(MDB2) algorithm using OpenMP.
 ********************************************************/
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <omp.h>
#include "pummaOpen.h"
double ElapsedTime(struct timeval startTime, struct timeval stopTime)
{
    long deltaSec, deltaUSec;

    if ((stopTime.tv_usec >= startTime.tv_usec) &&
        (stopTime.tv_sec >= startTime.tv_sec)){
      deltaSec = stopTime.tv_sec - startTime.tv_sec;
      deltaUSec = stopTime.tv_usec - startTime.tv_usec;
    }
    else if ((stopTime.tv_usec < startTime.tv_usec) &&
             (stopTime.tv_sec > startTime.tv_sec)){
      /* handle rollover - borrow */
      deltaSec = (stopTime.tv_sec - 1) - startTime.tv_sec;
      deltaUSec = (1000000 - startTime.tv_usec) + stopTime.tv_usec;
    }
    else{
      /* stopTime >= startTime */
      deltaSec = 0;
      deltaUSec = 0;
    }
    return ((double)deltaSec + ((double)deltaUSec/1000000.0));
}


void main( int argc, char *argv[] )
{
  int matrix_size;
  int 
    *ele_a,/* The matrix A */
    *ele_b,/* The matrix B */
    *temp_a, /* temporary array */
    *result;/* The result matrix C */
 
  int 
    lcm, /* LCM of the prcessors located in each column and row */
    num_processors, /* number of processors running */
    num_processor_x, /* number of processors running in a row or in a column */
    block_size; /* block size of the submatrix */
  int i,j,k,id,loop; /* loop variables */
  struct timeval startTime, stopTime;
  struct timezone tz; 
  char hname[30];
   
  if( argc != 2 ){
    printf( "Usage: %s <matrix_size>\n\n", argv[0] );
    exit(0);
  }
  matrix_size = atol(argv[1]);
  
  if( matrix_size <2 || matrix_size > MAX_ONED_ARRAY 
      || matrix_size %12 != 0){
    printf( "The matrix size must be between 2 and %d\n, and the matrix size must be the multiple of 12\n", MAX_ONED_ARRAY ); 
    fflush(stdout);
    exit(0);
  }
  
  else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_a= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (result= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ){
    printf( "Unable to allocate enough memory, exiting...\n");
    exit(0);   
  }
  for( i =0; i < matrix_size* matrix_size; i++){
    ele_a[i] = rand()%10;
    ele_b[i] = rand()%10;
    result[i]=0;
  }
  
  
  /* start timer */ 
  gettimeofday(&startTime, &tz);
#pragma omp parallel private (loop,i,j,k,id ) shared ( matrix_size, result, block_size,ele_a,ele_b )
   { 
     num_processors = omp_get_num_threads(); 
     if( num_processors == 1 ){
       num_processor_x = 1;
       lcm=1;
     }
     else if( num_processors == 4 ){
       num_processor_x = 2;
       lcm =2;
     }
     if( num_processors != 1 && num_processors != 4 ){
       printf( "The processors must be 1, or 4\n");
       exit(0);
     }
     block_size = matrix_size / num_processor_x;
     id = omp_get_thread_num() ;
     
     /*multiplication */
     for( loop=0; loop< lcm; loop++){
#pragma omp barrier
       for( i =0; i< block_size; i++){
	 for( j =0; j< block_size; j++){
	   /* calucation of P0 , and P1 processor */
	   if( id < num_processor_x ){
	     for( k=0; k< matrix_size; k+=lcm){
	       result[(i*num_processor_x)*matrix_size + j*num_processor_x+id] +=
		 ele_a[(i*num_processor_x)*matrix_size +id+k] *
		 ele_b[(id+loop+k)%matrix_size*matrix_size + j*num_processor_x+id];

	     }
	   }
	  
	   
	   /* calucation of P2, and P3 processor */
	   else{
	     for( k=0; k< matrix_size; k+=lcm){
	       result[(i*num_processor_x+1)*matrix_size + j*num_processor_x+id%2] +=
		 ele_a[(i*num_processor_x+1)*matrix_size +id%2+k] *
		 ele_b[(id%2+loop+k)%matrix_size*matrix_size + j*num_processor_x+id%2];	     }

	   }
	 }
       }
#pragma omp barrier
       if( id == 0 ){   
	 /*left-shift A */
	 for(i=0; i<matrix_size; i++)
	   for(j=0; j<matrix_size; j++){
	     temp_a[i*matrix_size+j]=ele_a[i*matrix_size+ (j+1)%matrix_size ];
	     
	   }
	 for(i=0; i<matrix_size; i++)
	   for(j=0; j<matrix_size; j++){
	     ele_a[i*matrix_size+j]=temp_a[i*matrix_size+j];
	   }   
	 
       }
     }
   }
   
   
   /* end timer */  
   gettimeofday(&stopTime, &tz);
      
   /* print result 
      printf( "\n\n result\n");
      for ( i = 0; i < matrix_size; i++){
      for ( j=0; j< matrix_size; j++ ){
      printf( " %d", result[i*matrix_size+j]);
      }
      printf( "\n");
      }
   */
 printf("%d proc multiplied ( %d ) matrix in %7.2f sec on node %s\n", 
	   num_processors, matrix_size,
	   ElapsedTime(startTime, stopTime), hname);
 /*   printf("%d proc on node %s : %7.2f sec\n", 
	  num_processors,
	  hname, ElapsedTime(startTime, stopTime));*/
   fflush(stdout);
   
   free( ele_a );
   free( ele_b );
   free( temp_a );
   free( result );
   
}










