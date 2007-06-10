
/*
#include "paRc_mpi.h"

void mpi_test(int *test) {
  
  int my_rank, p;
  MPI_Init((void *)0,(void *)0);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &p);
  if(my_rank == 0)
    *test = p;
  MPI_Finalize();
}
*/
