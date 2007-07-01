
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

## define benchmark
bench <- list()
bench$functions_to_be_applied <- c("serial.matrix.mult","serial.matrix.mult.native","mpi.matrix.mult","omp.matrix.mult")
bench$is_parallel <- c(FALSE,FALSE,TRUE,TRUE)
bench$cpu_range <- 1:maxcpu
bench$task <- "matrix multiplication"
bench$avail_cpu <- c(1,1,maxcpu,4)
bench$data1 <- matrix(runif(n*n,-5,5),nrow=n)
bench$data2 <- matrix(runif(n*n,-5,5),nrow=n)
class(bench) <- "benchmark"

bm_results <- run.benchmark(bench)
bm_results
save("bm_results",file="benchmark_results.Rda")
mpi.exit()


