
## load required libraries
library("Rmpi")
library("paRc")


## some information about the parallel environment
mpi.universe.size()

source("matrix_multiply.R")
n <- 1000
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

X <- mpi.matrix.mult(A,B,10)

mpi.exit()


