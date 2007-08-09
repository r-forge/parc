## Example: Matrix Multiplicatioon benchmark
## Run on shared memory machine
## includes also serial benchmarks

## load required libraries
library("paRc")

## some information about the parallel environment
omp.get.num.procs()
omp.get.max.threads()

## set cpu count to maximum possible in environment
maxcpu <- omp.get.max.threads()
## we want a maximum of 10 threads
if(maxcpu > 10)
  maxcpu <- 10

## set problem size (nxn matrix)
set.seed(1782)
n <- 1000
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)


## define benchmarks
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="OpenMP", parallel=TRUE, cpu_range=1:maxcpu)

bmres_OMP <- run.benchmark(bm)

bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="normal", parallel=FALSE, cpu_range=1)
bmres_normal <- run.benchmark(bm)

bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="native-BLAS", parallel=FALSE, cpu_range=1)
bmres_BLAS <- run.benchmark(bm)

bmres_shared <- rbind(bmres_normal,bmres_BLAS,bmres_OMP)
bmres_shared

save("bmres_shared",file="bmres_shared.Rda")



