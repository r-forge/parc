

/********************************************************/
/*             cannon's algorithm with OpenMP           */    
/********************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <omp.h>
#include "cannonOpen.h"

/*#include <sys/types.h>
#include <sys/resource.h>
*/


/*#include "times.h"
CPUDEFS
*/
double ElapsedTime(struct timeval startTime, struct timeval stopTime)
{
    long deltaSec, deltaUSec;

    if ((stopTime.tv_usec >= startTime.tv_usec) &&
        (stopTime.tv_sec >= startTime.tv_sec))
    {
        deltaSec = stopTime.tv_sec - startTime.tv_sec;
        deltaUSec = stopTime.tv_usec - startTime.tv_usec;
    }
    else if ((stopTime.tv_usec < startTime.tv_usec) &&
             (stopTime.tv_sec > startTime.tv_sec))
    {
        /* handle rollover - borrow */
        deltaSec = (stopTime.tv_sec - 1) - startTime.tv_sec;
        deltaUSec = (1000000 - startTime.tv_usec) + stopTime.tv_usec;
    }
    else
    {
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
    *ele_a,
    *ele_b,
    *temp_a, 
    *temp_b,
    *result;
 
  int num_processors, num_processor_x, block_size;
  int i,j,id,loop;
  /*double t0, t1;*/
  int timingOnly;
  struct timeval startTime, stopTime;
  struct timezone tz; 
  char hname[30];
  /* see if timingOnly enabled */
  timingOnly = (strcmp(argv[(argc - 1)], "timeONLY") == 0);
 
  if( argc != 2 ){
    printf( "Usage: %s <matrix_size>\n\n", argv[0] );
    exit(0);
  }
  matrix_size = atol(argv[1]);
  
  if( matrix_size <2 || matrix_size > MAX_ONED_ARRAY ){
    /*      || matrix_size %12 != 0){*/
    printf( "The matrix size must be between 2 and %d\n, and the matrix size must be the multiple of 12\n", MAX_ONED_ARRAY ); 
    fflush(stdout);
    exit(0);
  }
   else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	    (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	    (temp_a= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	    (temp_b= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	    (result= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ){
     printf( "Unable to allocate enough memory, exiting...\n");
     exit(0);   
   }
   for( i =0; i < matrix_size* matrix_size; i++){
      ele_a[i] = rand()%10;
      ele_b[i] = rand()%10;
      result[i]=0;
  }
   /*  for( i =0; i < matrix_size* matrix_size; i++){
      printf(" %d", ele_a[i]);
      if( i % matrix_size == matrix_size -1 )
	printf( "\n");
    }  printf( "\n");
    for( i =0; i < matrix_size* matrix_size; i++){
      printf(" %d", ele_b[i]);
      if( i % matrix_size == matrix_size-1 )
	printf( "\n");
	}*/  

  /* start timer */
   gettimeofday(&startTime, &tz);
 
  /* left circular shift of row i by i of Matrix A and
   * upward shift B */
  for(i=0; i<matrix_size; i++)
    /*#pragma omp parallel for shared (matrix_size, ele_a, ele_b )*/
    for(j=0; j<matrix_size; j++){
      temp_a[i*matrix_size+j]=ele_a[i*matrix_size+ (i+j) % matrix_size ];
      temp_b[i*matrix_size+j]=ele_b[((i+j) % matrix_size)*matrix_size+ j ];
   }

  for(i=0; i<matrix_size; i++)
    /*#pragma omp parallel for shared (matrix_size, temp_a, temp_b )*/
   for(j=0; j<matrix_size; j++){
      ele_a[i*matrix_size+j]=temp_a[i*matrix_size+j];
     ele_b[i*matrix_size+j]=temp_b[i*matrix_size+j];
   }
 
 /*multiplication */
#pragma omp parallel private (id,loop, i, j ) /*shared (block_size, ele_a, ele_b, result, matrix_size)*/
  {    
    num_processors = omp_get_num_threads(); 
    if( num_processors != 1 && num_processors != 4 ){
      printf( "The processors must be 1, or 4\n");
      exit(0);
    }
    if( num_processors == 1 )
      num_processor_x = 1;
    else if( num_processors == 4 )
      num_processor_x = 2;

    block_size = matrix_size / num_processor_x; 
    id = omp_get_thread_num() ;
    
    for( loop=0; loop< matrix_size; loop++){
#pragma omp barrier    
      for( i =0; i< block_size; i++){
	
	/* calucation of P0 , and P1 processor */
	if( id < num_processor_x ){
	  for( j =0; j< block_size; j++){
	    result[i*matrix_size + (id *block_size+j)] += 
	      ele_a[i*matrix_size + (id *block_size+j)]*
	      ele_b[i*matrix_size + (id *block_size+j)];
	  } 
	}
	/* calucation of P2, and P3 processor */
	else {
	  for( j =0; j< block_size; j++){
	    result[(i+block_size)*matrix_size + ((id-2)*block_size+j)] += 
	      ele_a[(i+block_size)*matrix_size + ((id-2)*block_size+j)] *
	      ele_b[(i+block_size)*matrix_size + ((id-2)*block_size+j)] ;
	  }
	}
	
      }
#pragma omp barrier        
      /*      printf("\n");*/
      
      
      if( id == 0 ) {
	/*	 printf("one loop  is complete\n");*/
	/*left-shift A and upward-shift B */ 
	for(i=0; i<matrix_size; i++)
	  /*#pragma omp parallel for shared (i, matrix_size, ele_a, ele_b )*/
	  for(j=0; j<matrix_size; j++){
	    temp_a[i*matrix_size+j]=ele_a[i*matrix_size+ (j+1) % matrix_size ];
	    temp_b[i*matrix_size+j]=ele_b[((i+1) % matrix_size)*matrix_size+ j ];
	    
	  }
	for(i=0; i<matrix_size; i++)
	  /*#pragma omp parallel for shared (i, matrix_size, temp_a, temp_b )*/
	  for(j=0; j<matrix_size; j++){
	    ele_a[i*matrix_size+j]=temp_a[i*matrix_size+j];
       ele_b[i*matrix_size+j]=temp_b[i*matrix_size+j];
	  }   
      }
    }
  }
  
  /* stop timer */
     gettimeofday(&stopTime, &tz);


 if ( !timingOnly )
    {
        /* print sorted data 
        printf("\n----\n\n");
        for (i = 0; i < numberOfProcesses; i++)
        {
            startIndex = subarrayStartIndex[i];
            length = subarrayLength[i];
            for (j = startIndex; j < (startIndex + length); j++)
                printf("%d\n", globalSortBuffer[j]);
            printf("\n----\n\n");
        }
	*/
    }

    gethostname(hname, 30);

    printf("%d proc multiplied ( %d ) matrix in %7.2f sec on node %s\n", 
	   num_processors, matrix_size,
	   ElapsedTime(startTime, stopTime), hname);
    fflush(stdout);
    /* end timer */  
    /*  t1 = CPUTIME;*/
  
  /* print result   
     printf( "\n\n result\n");
 for ( i = 0; i < matrix_size; i++){
   for ( j=0; j< matrix_size; j++ ){
       printf( " %d", result[i*matrix_size+j]);
   }
   printf( "\n");
 }
 */
    /* printf("%d processors: used %7.2f secs to multiply %d * %d matrices.\n",
       num_processors,t1 - t0, matrix_size, matrix_size);*/
    free( ele_a );
    free( ele_b );
    free( temp_a ); 
    free( temp_b );
    free( result );
    
}










