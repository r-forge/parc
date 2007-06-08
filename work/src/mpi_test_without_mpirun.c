// You have to look at MPI_Comm_spawn to create parallel threads
// otherwise use mpirun


#include <stdio.h>
#include "mpi.h"
#include <stdlib.h>

//static MPI_Comm *comm;
//static int COMM_MAXSIZE=10;

int main(void) {
  int flag, my_rank, p, i, *MPI_Universe_Size, univ_flag;
  MPI_Init((void *)0,(void *)0);
  MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
  MPI_Errhandler_set(MPI_COMM_SELF, MPI_ERRORS_RETURN);
  //comm = (MPI_Comm*) calloc(COMM_MAXSIZE, sizeof(MPI_Comm));
  //comm[0]=MPI_COMM_WORLD;	
  //for (i=1;i < COMM_MAXSIZE; comm[i++]=MPI_COMM_NULL);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &p);
  MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, &MPI_Universe_Size, &univ_flag);
  p = *MPI_Universe_Size;
  if(my_rank == 0)
      printf("%d, %d",my_rank,p);
  MPI_Finalize();
  MPI_Initialized(&flag);
  //free(comm);
  return 0;
}

