
## load required libraries
library("snow")
library("Rmpi")
library("paRc")

maxcpu <- mpi.universe.size()

## 1000x1000
set.seed(1782)
n <- 1000
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="snow-MPI", parallel=TRUE, cpu_range=1:maxcpu)

bmres_snowMPI <- run.benchmark(bm)
save("bmres_snowMPI",file="bmres_snowMPI-bignode-1000.Rda")


set.seed(1782)
n <- 2500
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="snow-MPI", parallel=TRUE, cpu_range=1:maxcpu)

bmres_snowMPI <- run.benchmark(bm)
save("bmres_snowMPI",file="bmres_snowMPI-bignode-2500.Rda")

## 5000x5000
set.seed(1782)
n <- 5000
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="snow-MPI", parallel=TRUE, cpu_range=1:maxcpu)

bmres_snowMPI <- run.benchmark(bm)
save("bmres_snowMPI",file="bmres_snowMPI-bignode-5000.Rda")

