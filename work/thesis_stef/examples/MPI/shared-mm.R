## load required libraries
library("Rmpi")
library("paRc")
library("snow")

## definition of benchmark run
maxcpu <- mpi.universe.size()
task <- "matrix multiplication"
taskID <- "mm"
paradigm <- "shared"
types <- c("MPI","snow-MPI","MPI-wB")
complexity <- c(1000,2500,5000)

bm <- create.benchmark(task=task, data=list(),
                       type=types[1], parallel=TRUE, cpu_range=1:maxcpu)

for(n in complexity){
  set.seed(1782)
  bmdata <- list()
  bmdata[[1]] <- matrix(runif(n*n,-5,5),nrow=n)
  bmdata[[2]] <- matrix(runif(n*n,-5,5),nrow=n)
  bm.data(bm) <- bmdata
  for(type in types){
    bm.type(bm) <- type
    writeLines(paste("Starting",type,"benchmark with complexity",n,"..."))
    results <- run.benchmark(bm) 

    save(results,file=paste(paste(paradigm,taskID,type,n,sep="-"),".Rda",sep=""))
  }
}

mpi.exit()
