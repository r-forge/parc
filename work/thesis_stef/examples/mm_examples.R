
## load libraries
library("paRc")
library("Rmpi")

## functions are in
source("matrix_multiply.R")
## serial examples
set.seed(1782)
n <- 500
A <- matrix(runif(n*n,-5,5),ncol=n)
B <- matrix(runif(n*n,-5,5),ncol=n)
system.time(A%*%B)
system.time(matrixmult.C(A,B))

## parallel examples

n_cpu <- 2
mpi.spawn.Rslaves(nslaves = n_cpu - 1)
mpi.comm.size() 
X<-matrix(1:16,nrow=4)
Y<-matrix(16:1,nrow=4)

serial_mm(X,Y)
parallel_mm(X,Y,n_cpu,spawnRslaves=FALSE)
X%*%Y

mpi.close.Rslaves()

## example test run serial vs. MPI implementation for 80x80 Matrix
n <- 80
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

n_cpu <- 4

mpi.spawn.Rslaves(nslaves = n_cpu - 1)

system.time(serial_mm(A,B))
system.time(parallel_mm(A,B,n_cpu))
system.time(A%*%B)

mpi.close.Rslaves()

## benchmark using a cpu range

## define Benchmark
bench <- list()
bench$functions_to_be_applied <- c("serial_mm","serial_mm_native","parallel_mm")
bench$is_parallel <- c(FALSE,FALSE,TRUE)
bench$cpu_range <- 1:4
bench$task <- "matrix multiplication"
bench$data1 <- matrix(runif(n*n,-5,5),nrow=n)
bench$data2 <- matrix(runif(n*n,-5,5),nrow=n)
class(bench) <- "benchmark"

run.benchmark(bench)

mpi.exit()


