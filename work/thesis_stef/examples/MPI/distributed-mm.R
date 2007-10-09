## load required libraries
library("Rmpi")
library("paRc")
library("snow")

## definition of benchmark run
maxcpu <- mpi.universe.size()
task <- "matrix multiplication"
taskID <- "mm-norm"
paradigm <- "distributed"
types <- c("MPI","snow-MPI")
complexity <- c(2500)
runs <- 1000
bmdata <- list()
bmdata[[1]] <- bmdata[[2]] <- 1000
bmdata[[3]] <- function(x){
  rnorm(x)
}

bm <- create.benchmark(task=task, data=bmdata,
                       type=types[1], parallel=TRUE, cpu_range=1:maxcpu, runs=runs)
set.seed(1782)
for(n in complexity){
  bmdata[[1]] <- bmdata[[2]] <- n
  bm.data(bm) <- bmdata
  for(type in types){
    bm.type(bm) <- type
    writeLines(paste("Starting",type,"benchmark with complexity",n,"..."))
    results <- run.benchmark(bm)      
    save(results,file=paste(paste(paradigm,taskID,type,n,sep="-"),".Rda",sep=""))
  }
}

