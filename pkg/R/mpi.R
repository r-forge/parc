## Test MPI

MPI_Test <- function () {

  # Check if lam-mpi is running
  if (.Platform$OS=="unix"){
    if (length(system("lamnodes",TRUE,ignore.stderr = TRUE)) == 0){
      cat("\n\tLAM/MPI runtime environment is not operating.\n")
    }
  }
	
  out <- .Call("mpi_test", test = integer(1), PACKAGE = "paRc")
  out$test
}


