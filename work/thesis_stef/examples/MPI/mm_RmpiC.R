
## load required libraries
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
                       type="MPI-wB", parallel=TRUE, cpu_range=1:maxcpu)

bmres_MPI <- run.benchmark(bm)
save("bmres_MPI",file="bmres_MPI-wB-shared-1000.Rda")

## 2500x2500
set.seed(1782)
n <- 2500
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="MPI-wB", parallel=TRUE, cpu_range=1:maxcpu)

bmres_MPI <- run.benchmark(bm)
save("bmres_MPI",file="bmres_MPI-wB-shared-2500.Rda")

## 5000x5000
set.seed(1782)
n <- 5000
bmdata <- list()
bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
bm <- create.benchmark(task="matrix multiplication", data=bmdata,
                       type="MPI-wB", parallel=TRUE, cpu_range=1:maxcpu)

bmres_MPI <- run.benchmark(bm)
save("bmres_MPI",file="bmres_MPI-wB-shared-5000.Rda")


mpi.exit()
