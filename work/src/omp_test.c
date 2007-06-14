/* Testing OpenMP capabilities
   This program prints the number of threads available on the machine
   theussl, 2007
*/

#include <stdio.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#if   _OPENMP == 200505
#  define OPENMPVER "2.5"
#else
#  define OPENMPVER "?.?"
#endif

int main(int argc, char *argv[])
{
#ifdef _OPENMP
  printf("OpenMP v%s (%d)\n", OPENMPVER, _OPENMP);
  printf("This machine has: %d processors.\n", omp_get_num_procs());
#pragma omp parallel
  {
  printf("This is thread %3d of %3d.\n",
	 omp_get_thread_num(),
	 omp_get_num_threads());
  }
#else
  printf("cannot run OpenMP.\n");
#endif
  return 0;
}
