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
    results <- run.benchmark(bm) 

    save(results,file=paste(paste(paradigm,taskID,type,n,sep="-"),".Rda",sep=""))
  }
}

