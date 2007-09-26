## load required libraries
library("rpvm")
library("paRc")
library("snow")

maxcpu <- 20

task <- "matrix multiplication"
taskID <- "mm"
paradigm <- "distributed"
types <- c("PVM","snow-PVM","PVM-wB")
complexity <- c(1000,2500,5000)
runs <- 10
bmdata <- list()
bmdata[[1]] <- bmdata[[2]] <- 1000
bmdata[[3]] <- function(x){
  runif(x,-5,5)
}

bm <- create.benchmark(task=task, data=list(),
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

