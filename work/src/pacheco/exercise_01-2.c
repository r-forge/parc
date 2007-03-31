/* This is from "Parallel Programming with MPI" from Peter Pacheco */
/* theussl,2006 */

#include <stdio.h>
#include <string.h>
#include <mpi.h>

int main(int argc, char* argv[]) {

  int my_rank;
  int p;
  int source;
  int dest;
  int tag=0;
  char message[100];
  MPI_Status status;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &p);
  
  if ((my_rank < p-1) && (my_rank > 0)){
     dest = my_rank - 1;
     sprintf(message, "Greetings from process %d!", my_rank);
     MPI_Send(message, strlen(message)+1, MPI_CHAR, dest, tag, MPI_COMM_WORLD);
     MPI_Recv(message, 100, MPI_CHAR, my_rank+1, tag, MPI_COMM_WORLD, &status);
     printf("This is process %d: %s\n",my_rank,message);
  }
   
  if (my_rank == p-1){
     dest = my_rank -1;
     sprintf(message, "Greetings from process %d!", my_rank);
     MPI_Send(message, strlen(message)+1, MPI_CHAR, dest, tag, MPI_COMM_WORLD);
     MPI_Recv(message, 100, MPI_CHAR, 0, tag, MPI_COMM_WORLD, &status);
     printf("This is process %d: %s\n",my_rank,message);
  }

  if (my_rank == 0){
     dest = p-1;
     sprintf(message, "Greetings from process %d!", my_rank);
     MPI_Send(message, strlen(message)+1, MPI_CHAR, dest, tag, MPI_COMM_WORLD);
     MPI_Recv(message, 100, MPI_CHAR, my_rank+1, tag, MPI_COMM_WORLD, &status);
     printf("This is process %d: %s\n",my_rank,message);
  }

  MPI_Finalize();
  return 0;
}
