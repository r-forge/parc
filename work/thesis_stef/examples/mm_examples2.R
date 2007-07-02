
## load libraries
library("paRc")

## source some function definitions
source("matrix_multiply.R")

## serial examples
set.seed(1782)
n <- 500
A <- matrix(runif(n*n,-5,5),ncol=n)
B <- matrix(runif(n*n,-5,5),ncol=n)
system.time(A%*%B)
system.time(serial.matrix.mult(A,B))

## parallel examples

n_cpu <- 2
X<-matrix(1:16,nrow=4)
Y<-matrix(16:1,nrow=4)

serial.matrix.mult(X,Y)
omp.matrix.mult(X,Y,n_cpu)
X%*%Y

## example test run serial vs. OpenMP implementation for 80x80 Matrix
n <- 1000
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

n_cpu <- 4

system.time(serial.matrix.mult(A,B))
system.time(omp.matrix.mult(A,B,n_cpu))
system.time(A%*%B)

## benchmark using a cpu range

## define Benchmark
bench <- list()
bench$functions_to_be_applied <- c("serial.matrix.mult","serial.matrix.mult.native","omp.matrix.mult")
bench$is_parallel <- c(FALSE,FALSE,TRUE)
bench$cpu_range <- 1:4
bench$task <- "matrix multiplication"
bench$avail_cpu <- c(1,1,4)
bench$data <- list()
bench$data[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bench$data[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
class(bench) <- "benchmark"

results <- run.benchmark(bench)
results
