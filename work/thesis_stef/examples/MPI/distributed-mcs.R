## load required libraries
library("Rmpi")
library("paRc")

## definition of benchmark run
maxcpu <- mpi.universe.size()
task <- "Monte Carlo simulation"
taskID <- "mcs"
paradigm <- "distributed"
types <- c("MPI")
#complexity <- c(1000,2500,5000)

bm <- create.benchmark(task=task, data=list(),
                       type=types[1], parallel=TRUE, cpu_range=1:maxcpu)
## build option
opt <- list()
opt$mu <- 0.1           ## expectation of underlying
opt$sigma <- 0.4        ## standard deviation of underlying
opt$type <- "Call"      ## type of the option
opt$strikeprice <- 100  ## strikeprice of the option
opt$present <- 100      ## present value of the underlying
opt$maturity <- 1/12    ## time to maturity (in years)
opt <- as.option(opt)   ## coercing 'list' to 'option'

bmdata <- list()
bmdata[[1]] <- opt
bmdata[[2]] <- 0.1 ## yield
bmdata[[3]] <- 30  ## n
bmdata[[4]] <- 5000 ## length
bmdata[[5]] <- 50  ## number of simulations
bmdata[[6]] <- TRUE ## use antithetic
bm.data(bm) <- bmdata

#for(n in complexity){
#  set.seed(1782)

  for(type in types){
    bm.type(bm) <- type
    writeLines(paste("Starting",type,"benchmark..."))
    results <- run.benchmark(bm) 

    save(results,file=paste(paste(paradigm,taskID,type,sep="-"),".Rda",sep=""))
  }
#}

