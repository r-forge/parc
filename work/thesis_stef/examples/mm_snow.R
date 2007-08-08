## load required libraries
library("snow")
library("paRc")
library("Rmpi")


## some information about the parallel environment
mpi.universe.size()
mpi.get.processor.name()
mpi.is.master()

## define functions

set.seed(1782)
## set cpu count to maximum possible in environment
maxcpu <- mpi.universe.size()
## maximum Rmpi can spawn is 10 Rslaves
if(maxcpu > 10)
  maxcpu <- 10
## set problem size (nxn matrix)
n <- 100

## define benchmark
bench <- list()
bench$functions_to_be_applied <- c("snow.matrix.mult")
bench$is_parallel <- c(TRUE)
bench$cpu_range <- 1:maxcpu
bench$task <- "matrix multiplication"
bench$avail_cpu <- c(1,1,maxcpu)
bench$data <- list()
bench$data[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bench$data[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
class(bench) <- "benchmark"

bm_results <- run.benchmark(bench)
bm_results
save("bm_results",file="Rmpi_results.Rda")
mpi.exit()


