/**************************************************************************
 * PUMMA(MDB1) algorithm with two dimensional scattered processor template.
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
    *block_a,
    *temp_a,
    *temp_d0,
    *temp_d1,
    *temp_d2,
    *temp_d3,
    *diagonal,
    *temp_result,
    *c;
  

  int a,b,d,e,i, j, k,l, loop_x, loop_y, loop, shift;
  
  char  name[100];     
  
  int
    matrix_size,
    lcm,
    d_length,
    kp,
    jump,
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

  scanf("%d", &matrix_size);
  if( matrix_size <2 || matrix_size > MAX_ONED_ARRAY ){
    /*      || matrix_size %12 != 0){*/
    if( my_node == HOST ){
      printf( "The matrix size must be between 2 and %d\n, and the matrix size must be the multiple of 12\n", MAX_ONED_ARRAY ); 
      fflush(stdout);
    }
    
    exit(0);
  }
  if( num_processors == 2 &&
      num_processors == 3 &&
      num_processors == 5 &&
      num_processors == 7 &&
      num_processors == 8 &&
      num_processors == 10 &&
      num_processors == 11 &&
      num_processors == 13 &&
      num_processors == 14 &&
      num_processors == 15 ){
    printf("The number of processor should be 1, 4, 6, 9, 12, or 16\n");
    exit(0);
  } 
  else if( (ele_a = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (ele_b = malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_a= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_d0= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_d1= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_d2= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (temp_d3= malloc( sizeof(int) * matrix_size * matrix_size ))== 0 ||
	   (block_a=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
	   (diagonal=malloc( sizeof(int)*matrix_size*matrix_size ))== 0 ||
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
    lcm = 1;
  }
  if( num_processors == 4 ){
    num_processor_x = 2;
    num_processor_y = 2;
    lcm = 2;
  }
  if( num_processors == 6 ){
    num_processor_x = 3;
    num_processor_y = 2;
    lcm = 6;
  } 
  if( num_processors == 9 ){
    num_processor_x = 3;
    num_processor_y = 3;
    lcm = 3;
  }
  if( num_processors == 12 ){
    num_processor_x = 4;
    num_processor_y = 3;
    lcm = 12;
  }
  if( num_processors == 16 ){
    num_processor_x = 4;
    num_processor_y = 4;
    lcm = 4;
  }


   
  block_size_x = matrix_size / num_processor_x;
  block_size_y = matrix_size / num_processor_y;
  d_length = matrix_size * matrix_size / num_processor_x / num_processor_x;
  
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
    /*   for(i=0; i<matrix_size; i++){
      for( j=0; j<matrix_size; j++){
	printf( " %d", ele_a[i*matrix_size+j]);
      }
      printf("\n");
    }
    printf("\n");
    for(i=0; i<matrix_size; i++){
      for( j=0; j<matrix_size; j++){
	printf( " %d", ele_b[i*matrix_size+j]);
      }
      printf("\n");
    }
    */
    /* set matrix to scatter mode */
    k=0;
    for( loop_y = 0; loop_y<  matrix_size/block_size_y; loop_y++ ){
      for( loop_x = 0; loop_x<  matrix_size/block_size_x; loop_x++ ){
	for(i=loop_y; i<matrix_size; i += num_processor_y){
	  for( j=loop_x; j<matrix_size; j += num_processor_x){
	    temp_a[k]=ele_a[i*matrix_size+ j];
	    k++;
	  }
	}
      }
    }
    
    /* Scatter elements and set its submatrix A which is block_a */
    for( i=1; i< num_processors; i++){
      MPI_Send( &temp_a[i*block_size_x * block_size_y ],
		block_size_x * block_size_y,
		MPI_INT, i, DATA_A, MPI_COMM_WORLD );
    }
    
    k=0;
    for( i = 0; i < block_size_y; i++)
      for( j = 0; j < block_size_x; j++){
	block_a[i*block_size_x+j] = temp_a[k];
	k++;
      }
  }
  
  /*receive data from host */
  else{
    MPI_Recv( block_a, MAX_ONED_ARRAY, MPI_INT, HOST, DATA_A,
	      MPI_COMM_WORLD, &Status);
    MPI_Get_count(&Status, MPI_INT, &length);
  }  
  /* initialize the result matrix */
  for( i=0; i < block_size_x*block_size_y; i++){
    temp_result[i]=0;
  } 

  /* set Processor coordinator */
  if( num_processors == 1 ){
    p_x = 0;
    p_y = 0;
  }
  if( num_processors == 4 ){
    p_x = my_node %2 ;
    p_y = (my_node  < 2 ) ? 0:1;
  }
  if( num_processors == 6){
    p_x = my_node %3 ;
    p_y = (my_node  < 3 ) ? 0:1;
  }
  if( num_processors == 9 ){
    p_x = my_node %3 ;
    if( my_node < 3 ) p_y = 0;
    else if ( my_node <6 ) p_y = 1;
    else p_y = 2;
  }
  if( num_processors == 12 ){
    p_x = my_node %4 ;
    if( my_node < 4 ) p_y = 0;
    else if ( my_node <8 ) p_y = 1;
    else p_y = 2;
  }
  if( num_processors == 16 ){
    p_x = my_node %4 ;
    if( my_node < 4 ) p_y = 0;
    else if ( my_node <8 ) p_y = 1;
    else if ( my_node <12 ) p_y = 2;
    else p_y = 3;
  }
  


  for( shift = 0; shift < num_processor_x; shift++ ){
    /*modify*/
    /******************************************************************/ 
    /* Find the diagonal elements and broadcast it to the processors. */
    if( my_node == HOST ){
      k=0;
      for( l=0; l< matrix_size/num_processor_x; l++)
	for( j=0; j< num_processor_x; j++){
	  for( i = 0; i < matrix_size; i+= num_processor_x ){
	    diagonal[k] = 
	      ele_b[(i+j+shift)%matrix_size*matrix_size+j + num_processor_x*l];
	    k++;
	  }
	}

      if( num_processors == 1 )
	for( i=0; i< matrix_size *matrix_size/lcm; i++)
	  temp_d0[i] = diagonal[i];
    
      /* set diagonal elements when 4 processors running */
      if( num_processors == 4 ){
	l=0;
	a=0;
	b=0;
	for( i=0; i< matrix_size/lcm; i++){
	  
	  for( j=0; j< matrix_size/lcm; j++){
	    temp_d0[a] = diagonal[l];
	    l++;
	    a++;
	  }
	  for( j=0; j< matrix_size/lcm; j++){
	    temp_d1[b] = diagonal[l];
	    l++;
	    b++;
	  }
	}
	MPI_Send( &temp_d0[0], d_length,
		  MPI_INT, 2, DIAGONAL, MPI_COMM_WORLD );     
	for( i=1; i< num_processors; i+= num_processor_x){
	  MPI_Send( &temp_d1[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	}
      }	  
      /* set/send diagonal elements when 6 or 9 processors running */
      if( num_processors == 6 || num_processors == 9){
	l=0;
	a=0;
	b=0;
	d=0;
	for( i=0; i< matrix_size/num_processor_x; i++){
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d0[a] = diagonal[l];
	    l++;
	    a++;
	  }
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d1[b] = diagonal[l];
	    l++;
	    b++;
	  }
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d2[d] = diagonal[l];
	    l++;
	    d++;
	  }

	}
	for( i=num_processor_x; i< num_processors; i+= num_processor_x)
	  MPI_Send( &temp_d0[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	for( i=1; i< num_processors; i+= num_processor_x){
	  MPI_Send( &temp_d1[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	}
	for( i=2; i< num_processors; i+= num_processor_x){
	  MPI_Send( &temp_d2[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	}
      }	  
	/* set diagonal elements when 12 or 16 processors running */
      if( num_processors == 12 || num_processors == 16 ){
	l=0;
	a=0;
	b=0;
	d=0;
	e=0;
	for( i=0; i< matrix_size/num_processor_x; i++){
	  
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d0[a] = diagonal[l];
	    l++;
	    a++;
	  }
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d1[b] = diagonal[l];
	    l++;
	    b++;
	  }
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d2[d] = diagonal[l];
	    l++;
	    d++;
	  }
	  for( j=0; j< matrix_size/num_processor_x; j++){
	    temp_d3[e] = diagonal[l];
	    l++;
	    e++;
	  }
	}
	/*
	printf("\nSend temp_0");
	for( i=0; i< d_length; i++)
	  printf(" %d", temp_d0[i]);
	printf("\n");
	*/	
	for( i=num_processor_x; i< num_processors; i+= num_processor_x)
	  MPI_Send( &temp_d0[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	for( i=1; i< num_processors; i+= num_processor_x)
	  MPI_Send( &temp_d1[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	for( i=2; i< num_processors; i+= num_processor_x)
	  MPI_Send( &temp_d2[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
	
	for( i=3; i< num_processors; i+= num_processor_x)
	  MPI_Send( &temp_d3[0], d_length,
		    MPI_INT, i, DIAGONAL, MPI_COMM_WORLD );     
      }	  
    }
        
    else{
      MPI_Recv( temp_d0, MAX_ONED_ARRAY, MPI_INT, HOST, DIAGONAL,
		MPI_COMM_WORLD, &Status);
    }
    /*    if( my_node == 2){
      printf(" node: %d", my_node );
      for(i=0; i< d_length; i++){
	printf( " %d", temp_d0[i]);
      }
      printf("\n");
    */
    
  
    /* multiply */
    for( i =0; i < block_size_y; i ++){
      for( j=0; j < block_size_x; j ++){
	for( k= 0; k < block_size_x; k ++){
	  temp_result[i*block_size_x+j] += 
	    block_a[i*block_size_x+k] * temp_d0[block_size_x*j+k];	 
	  /*    if( my_node == 0 )
		printf( " %d*%d=%d",  block_a[i*block_size_x+k],
		temp_d0[matrix_size/lcm*j+k],
		temp_result[i*block_size_x+j]);
	  */
	}
      }
    }
    
    /* multiplication step when 6 processors running 
    if( num_processors == 6 || num_processors == 12 )
      for( i =0; i < block_size_y; i ++){
	for( j=0; j < block_size_x; j ++){
	  for( k= 0; k < matrix_size/num_processor_x; k ++){
	    temp_result[i*block_size_x+j] += 
	      block_a[i*block_size_x+k] * temp_d0[block_size_x*j+k];	 
	    if( my_node == 0 )
	      printf( " %d*%d=%d",  block_a[i*block_size_x+k],
		      temp_d0[block_size_x*j+k],
		      temp_result[i*block_size_x+j]);
    */
    /*
      if( j%2==0 ){
      temp_result[i*block_size_x+j] += 
      block_a[i*block_size_x+k*2] * temp_d0[j*matrix_size/num_processor_x+k];
    */      /* 
	       if( my_node == 0 )
	       printf( " %d*%d=%d(i:%d,j:%d,k:%d)", 
	       block_a[i*block_size_x+k*2],
	       temp_d0[j*matrix_size/num_processor_x+k],
	       temp_result[i*block_size_x+j],
	       i,j,k);
	    */
    
    /* multiplication of the minor row and minor diagonal */
    /*else{
      temp_result[i*block_size_x+j] += 
      block_a[i*block_size_x+2*k+1]*temp_d0[j*matrix_size/num_processor_x+k];
    *//*if( my_node == 0 )
	printf( " %d*%d=%d(i:%d,j:%d,k:%d)", 
	block_a[i*block_size_x+2*k+1],
	temp_d0[j*matrix_size/num_processor_x+k],
	temp_result[i*block_size_x+j],
	i,j,k);*/
  
    
    /* multiplication step when 9 processors running 
       if( num_processors == 12)
       for( i = 0; i < block_size_y; i ++){
       for( j = 0; j < block_size_x; j ++){
       for( k= 0; k < matrix_size/num_processor_x; k ++){
       if( j%3==0 ){
	      temp_result[i*block_size_x+j] += 
	      block_a[i*block_size_x+k*3] * temp_d0[j*matrix_size/num_processor_x+k];
	      
	      }
	      else if( j%3 == 1 ){
	      temp_result[i*block_size_x+j] += 
	      block_a[i*block_size_x+3*k+1]*temp_d0[j*matrix_size/num_processor_x+k];
	      }
	      else if( j%3 == 2 ){
	      temp_result[i*block_size_x+j] += 
	      block_a[i*block_size_x+3*k+2]*temp_d0[j*matrix_size/num_processor_x+k];
	      }
	      
	      
	      }
	      }
	      }
    */
    /* Set the matrix A to 1D array and send it to the previous processor */
    k=0;
    for( i = 0; i < block_size_y; i++){
      for( j = 0; j < block_size_x; j++){
	if ( p_x != 0                 )
	  temp_a[k] =  block_a[i*block_size_x+j];
	else {
	  temp_a[k] =  block_a[ i *block_size_x+(j + 1 ) % block_size_x];
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
    
    
    MPI_Send( &temp_a[0], block_size_x* block_size_y,
	      MPI_INT, left_p,
	      A, MPI_COMM_WORLD );
    MPI_Recv( block_a, MAX_ONED_ARRAY, MPI_INT, right_p, A,
	      MPI_COMM_WORLD, &Status); 
  }
  
  /* Send result */
  if( my_node != HOST ){
    MPI_Send( &temp_result[0], block_size_y * block_size_x, 
	      MPI_INT, HOST,
	      C, MPI_COMM_WORLD );
  }

  else{
    /* Set the result matrix calculated by HOST */
    l=0;
    for( k=0; k< block_size_x * block_size_y; k++){
      if( k % block_size_x != (block_size_x-1) ){
	c[l] = temp_result[k];
	l += num_processor_x;
      }
      else{
	c[l] = temp_result[k];
	l += num_processor_x;
	l += matrix_size*( num_processor_y -1 ) ;
      }
    }
    /* receive the result from the processor */
    for( loop = 1; loop < num_processors; loop++ ){
      MPI_Recv( temp_result, MAX_ONED_ARRAY, MPI_INT,
		loop, C,
		MPI_COMM_WORLD, &Status );
      MPI_Get_count( &Status, MPI_INT, &templen );
      
      if( loop < num_processor_x ){
	l = loop;
	for( k =0; k < templen; k++){
	  if( (k % block_size_x) != (block_size_x-1) ){
	    c[l] = temp_result[k];
	    l += num_processor_x;
	  }
	  else{
	    c[l] = temp_result[k];
	    l += num_processor_x;
	    l += matrix_size*( num_processor_y -1 ) ;
	  }
	}
      }
      
      else if( loop < 2*num_processor_x ){
	l = loop + matrix_size - num_processor_x;
	for( k =0; k < templen; k++){
	  if( k % block_size_x != (block_size_x-1) ){
	    c[l] = temp_result[k];
	    l += num_processor_x;
	  }
	  else{
	    c[l] = temp_result[k];
	    l += num_processor_x;
	    l += matrix_size*( num_processor_y -1 ) ;
	  }
	}
      }  
      else if( loop < 3*num_processor_x ){
	l = loop + 2*matrix_size - 2*num_processor_x;
	for( k =0; k < templen; k++){
	  if( k % block_size_x != (block_size_x-1) ){
	    c[l] = temp_result[k];
	    l += num_processor_x;
	  }
	  else{
	    c[l] = temp_result[k];
	    l += num_processor_x;
	    l += matrix_size*( num_processor_y -1 ) ;
	  }
	}
      }
      else if( loop < 4*num_processor_x ){
	l = loop + 3*matrix_size - 3*num_processor_x;
	for( k =0; k < templen; k++){
	  if( k % block_size_x != (block_size_x-1) ){
	    c[l] = temp_result[k];
	    l += num_processor_x;
	  }
	  else{
	    c[l] = temp_result[k];
	    l += num_processor_x;
	    l += matrix_size*( num_processor_y -1 ) ;
	  }
	}
      }

    }
  
  getrusage(RUSAGE_SELF, &resources);
  time = resources.ru_utime.tv_sec * 1000000 + resources.ru_utime.tv_usec 
    - time;
  
  endTime = MPI_Wtime() - startTime;
  
  /*
 printf( "result aa\n");
  for( k = 0; k < matrix_size * matrix_size; k++){
    if( k % matrix_size == 0 )
      printf( "\n");
    printf( " %d", c[k] );
    
  }	  printf( "\n");
  */
  printf("\n\ntime=%6.6f seconds/%6.6f seconds\n\n",
	 (float)time/1000000.0, endTime);
  
  fflush(stdout);
  

  }
  MPI_Barrier(MPI_COMM_WORLD);
  free(ele_a );
  free(ele_b );
  free(block_a );
  free(temp_a );
  free(temp_d0 );
  free(temp_d1 );
  free(temp_d2 );
  free(temp_d3 );
  free(diagonal );
  free(temp_result );
  free(c);
  


}
  
  void main(int argc,char *argv[])
    {
  /*  Set up mpi machine */
  MPI_Init(&argc, &argv);
 
MULnode( argc, argv );

  MPI_Finalize();
}









