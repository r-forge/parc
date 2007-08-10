## benchmark.R
## Stefan Theussl, 2007

## creates a benchmark object

create.benchmark <- function(task, data, type="normal", parallel=FALSE, cpu_range=1){
  out <- list()
  tasks <- c(
             "matrix multiplication",
             "Monte Carlo Simulation"
             )
  if(is.null(task)) stop("No task chosen to benchmark")
  else taskNr <- pmatch(tolower(task), tolower(tasks))
  if(is.na(taskNr)) stop (paste("Unknown task:",sQuote(task)))

  out$task <- taskNr
  out$tasks <- tasks
  
  types <- c(
             "normal",
             "native-BLAS",
             "goto-BLAS",
             "MKL-BLAS",
             "OpenMP",
             "MPI",
             "PVM",
             "snow-MPI",
             "snow-PVM")

  typeNr <- pmatch(tolower(type), tolower(types))
  if(is.na(typeNr)) stop (paste("Unknown type:",sQuote(type)))

  out$type <- typeNr
  out$types <- types
  
  if(!is.logical(parallel))
    stop("'parallel' must be either TRUE or FALSE")
  out$is_parallel <- parallel
  if(!(all(cpu_range > 0) && is.numeric(cpu_range)))
    stop("'cpu_range' must be a vector of positive integers")
  out$cpu_range <- as.integer(cpu_range)
  
  if(!is.list(data))
    stop("'data' must be of type list")
  out$data <- data
  
  class(out) <- "benchmark"
  out
}

## main benchmark function
run.benchmark <- function(x){
  ## input validation
  if(class(x)!="benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(bm.task(x), bm.tasks(x))

  if(taskNr == 1) {
    results <- bm.matrix.multiplication(x)
  }else if(taskNr == 2) {
    writeLines("Not implemented yet")
    results <- NULL
  }
  results
}


## extractor functions

bm.task <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks[x$task]
}

bm.tasks <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks
}

bm.type <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$types[x$type]
}

bm.types <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$types
}

## TODO: generic
bm.is.parallel <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$is_parallel
}

bm.cpu.range <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$cpu_range
}

bm.data <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$data
}

## replacement functions



bm.data.frame <- function(){
  out <- data.frame(task=NA, type=NA, n_cpu=NA, time_usr=NA, time_sys=NA,
                    time_ela=NA, is_parallel=NA)
  class(out) <- c("bm_results",class(out))
  out
}


bm.function.to.apply <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(bm.task(x), bm.tasks(x))
  type <- bm.type(x)
  
  if(taskNr==1)
    foo <- switch(type,
                  "normal" = serial.matrix.mult,
                  "native-BLAS" = function(X,Y){X%*%Y},
                  "goto-BLAS" = function(X,Y){X%*%Y},
                  "MKL-BLAS" = function(X,Y){X%*%Y},
                  "OpenMP" = omp.matrix.mult,
                  "snow-MPI" = function(X,Y,n = 1){
                    if(n==1)
                      out <- X%*%Y
                    else{
                      cl <- getMPIcluster()
                      out <- parMM(cl, X, Y)
                    }
                    out
                  },
                  "MPI" = mm.Rmpi,
                  stop("no such type")
                  )
  else
    stop("no such task")
  foo
}

bm.prepare <- function(x,n){
  type <- bm.type(x)
  if(any(type == c("normal", "native-BLAS", "goto-BLAS", "MKL-BLAS", "OpenMP")))
    return()
  if(type == "snow-MPI"){
    if(!(require("snow")&&require("Rmpi")))
      stop("Packages 'Rmpi' and 'snow' are required to run this benchmark")
    cl <- makeCluster(n, type = "MPI")
  }
  if(type == "MPI"){
    if(!(require("Rmpi")))
      stop("Packages 'Rmpi' is required to run this benchmark")
    mpi.spawn.Rslaves(nslaves=n)
  }
}

bm.close <- function(x){
  type <- bm.type(x)
  if(any(type == c("normal", "native-BLAS", "goto-BLAS", "MKL-BLAS", "OpenMP")))
    return()
  if(type == "snow-MPI"){
    cl <- getMPIcluster()
    stopCluster(cl)  
  }
  if(type == "MPI")
    mpi.close.Rslaves()
}
## benchmark workhorses

bm.matrix.multiplication <- function(x){
  ## build dataframe for results
  out <- bm.data.frame()
  
  data <- bm.data(x)
  if(!length(data)==2)
    stop("'data' supplied must be a list containing two matrices")
  foo <- bm.function.to.apply(x)  
  for( n_cpu in bm.cpu.range(x)){
    if(n_cpu == 1)
      out[n_cpu,] <- c(bm.task(x),bm.type(x),n_cpu,
                   as.vector(system.time(foo(data[[1]],data[[2]])))[1:3],
                   bm.is.parallel(x))
    else {
      bm.prepare(x,n_cpu)
      tmp <- c(bm.task(x), bm.type(x), n_cpu,
               as.vector(system.time(foo(data[[1]], data[[2]], n_cpu)))[1:3],
               TRUE)
      out[n_cpu,] <- tmp
      bm.close(x)     
    }
  }
  ## format data.frame accordingly
  out$time_usr <- as.numeric(out$time_usr)
  out$time_sys <- as.numeric(out$time_sys)
  out$time_ela <- as.numeric(out$time_ela)
  out$is_parallel <- as.logical(out$is_parallel)
  out
}

## S3 methods

## print method
print.benchmark <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  run <- ifelse(bm.is.parallel(x),"parallel","serial")
  task <- bm.task(x)
  type <- bm.type(x)
  writeLines(paste("A", run, "benchmark running task:", task, "-", type))
}

## plot method (requires vcd)

require("vcd")

plot.bm_results <- function(x, task="all", ... ){
  if(!class(x)[1]=="bm_results")
    stop("'x' not of class 'bm_results'")

  ntypes <- length(unique(x$type))
  ntasks <- length(unique(x$task))

  if(ntypes > 5)
    stop("more than 5 functions in a benchmark are not supported yet")

  
  ## define plot region, colorspace and other plot parameters
  xlim <- c(0,max(as.numeric(x$n_cpu))+1)
  ylim <- c(0,max(x$time_ela)+1)
  ncolors <- ntypes*ntasks
  colors <- rainbow_hcl(ncolors, c=80, l=65, start = 20, end = 340)
  ltys <- c(1:6)
  pchs <- c(21:25)
  par(mar=c(5,4,4,5))
  main=paste("Task:",task)

  ## plot reference
  ref <- x$type[1]
  plot( x = as.numeric(x$n_cpu)[1], y = x$time_ela[1], col=colors[1], xlim = xlim, ylim = ylim, type = "b",
       pch = pchs[1], ,xlab = "# of CPUs", ylab = "execution time", main = main)

  ##plot the rest
  results.to.plot <- unique(x$type)
  results.to.plot <- results.to.plot[-which(results.to.plot==ref)]
  for(i in 1:length(results.to.plot)){
    lines(x = as.numeric(x$n_cpu[which(x$type==results.to.plot[i])]), y = x$time_ela[which(x$type==results.to.plot[i])],
          col=colors[i+1], type = "b", lty= ltys[i+1], pch = pchs[i+1])
  }

  ## plot speedup
  ##par(new = TRUE)
  ##xlim <- c(0,x$cpucount[x$cpusteps]+1)
  ##ylim <- c(0,max(x$speedup))
  ##plot(x$cpucount, x$speedup[1,], col = colors[1+x$kinds], axes = FALSE, xlim = xlim, ylim=ylim, type = "b", xlab = "", ylab = "", lty=ltys[1], pch = pchs[1])
  ##for(i in 2:x$kinds)
  ##  lines(x = x$cpucount, y = x$speedup[i,], col=colors[i+x$kinds], type = "b", lty= ltys[i], pch = pchs[i])
  ##axis(4)
  ##mtext("Speedup", side = 4, line = 3)

  ## legend
  legend("topleft", c(ref,results.to.plot), col=colors[1:ntypes], lty = ltys[1:ntypes], bty = "n", pch = pchs[1:ntypes])
}
