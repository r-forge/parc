## load required libraries
library("Rmpi")
library("paRc")

## some information about the parallel environment
mpi.universe.size()
mpi.get.processor.name()
mpi.is.master()

## define functions

source("matrix_multiply.R")

set.seed(1782)
## set cpu count to maximum possible in environment
maxcpu <- mpi.universe.size()
## maximum Rmpi can spawn is 10 Rslaves
if(maxcpu > 10)
  maxcpu <- 10

## set problem size (nxn matrix)
n <- 100
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)


## define benchmark
bm <- create.benchmark(task="matrix multiplication", data=bmdata, type="MPI",
                       parallel=TRUE, cpu_range=1:maxcpu)

bmres_MPI <- run.benchmark(bm)
bmres_MPI
save("bmres_MPI",file="bmres_MPI.Rda")
mpi.exit()


