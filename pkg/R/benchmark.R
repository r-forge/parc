## benchmark.R
## Stefan Theussl, 2007

## creates a benchmark object

create_benchmark <- function(task, data, type="normal", parallel=FALSE, cpu_range=1, runs=1){
  out <- list()
  tasks <- c(
             "matrix multiplication",
             "Monte Carlo simulation"
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
             "snow-PVM",
             "MPI-wB",
             "PVM-wB")

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

  if(!(all(runs > 0) && is.numeric(runs)))
    stop("'runs' must be a  positive integer")
  out$runs <- as.integer(runs)
  
  
  if(!is.list(data))
    stop("'data' must be of type list")
  out$data <- data
  
  class(out) <- "benchmark"
  out
}

## main benchmark function
run_benchmark <- function(x){
  ## input validation
  if(class(x)!="benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(benchmark_task(x), benchmark_tasks(x))
    
  if(taskNr == 1) {
    results <- benchmark_matrix_multiplication(x)
  }else if(taskNr == 2) {
    results <- benchmark_Monte_Carlo_simulation(x)
  }
  results
}


## extractor functions

benchmark_task <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks[x$task]
}

benchmark_tasks <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks
}

benchmark_runs <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$runs
}

benchmark_type <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$types[x$type]
}

benchmark_types <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$types
}

## TODO: generic
benchmark_is_parallel <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$is_parallel
}

benchmark_cpu_range <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$cpu_range
}

benchmark_data <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  n <- x$data[[1]]
  m <- x$data[[2]]
  foo <- x$data[[3]]
  dat <- list()
  dat[[1]] <- matrix(foo(n*m),nrow=n)
  dat[[2]] <- matrix(foo(m*n),ncol=n)
  dat                    
}

## replacement functions

'benchmark_task<-' <- function(x, value){
  if(!inherits(x, "benchmark"))
    stop("'x' not of class 'benchmark'")
  tasks <- c(
             "matrix multiplication",
             "Monte Carlo simulation"
             )
  if(is.null(value)) stop("No task chosen to benchmark")
  else taskNr <- pmatch(tolower(value), tolower(tasks))
  if(is.na(taskNr)) stop (paste("Unknown task:",sQuote(value)))
  x$task <- taskNr
  x
}

'benchmark_type<-' <- function(x, value){
  if(!inherits(x, "benchmark"))
    stop("'x' not of class 'benchmark'")
  types <- benchmark_types(x)
  typeNr <- pmatch(tolower(value), tolower(types))
  if(is.na(typeNr)) stop (paste("Unknown type:",sQuote(value)))
  x$type <- typeNr
  x
}

'benchmark_cpu_range<-' <- function(x, value) {
  if(!inherits(x, "benchmark"))
    stop("'x' not of class 'benchmark'")
  if(!(all(value > 0) && is.numeric(value)))
    stop("'cpu_range' must be a vector of positive integers")
  x$cpu_range <- value
  x
}

'benchmark_data<-' <- function(x, value) {
  if(!inherits(x, "benchmark"))
    stop("'x' not of class 'benchmark'")
  if(!is.list(value))
    stop("'value' must be of type list")
  x$data <- value
  x
}

## internal functions

benchmark_data.frame <- function(){
  out <- data.frame(task=NA, type=NA, n_cpu=NA, time_usr=NA, time_sys=NA,
                    time_ela=NA, is_parallel=NA, run=NA)
  class(out) <- c("benchmark_results",class(out))
  out
}


benchmark_function_to_apply <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(benchmark_task(x), benchmark_tasks(x))
  type <- benchmark_type(x)
  
  if(taskNr==1){
    foo <- switch(type,
                  "normal" = serial_matrix_multiplication,
                  "native-BLAS" = function(X,Y){X%*%Y},
                  "goto-BLAS" = function(X,Y){X%*%Y},
                  "MKL-BLAS" = function(X,Y){X%*%Y},
                  "OpenMP" = omp_matrix_multiplication,
                  "snow-MPI" = function(X,Y,n = 1){
                    if(n == 1)
                      out <- X%*%Y
                    else{
                      cl <- getMPIcluster()
                      out <- parMM(cl, X, Y)
                    }
                    out
                  },
                  "snow-PVM" = function(X,Y,cl=1){
                    if(!is.list(cl))
                      out <- X%*%Y
                    else{
                      out <- parMM(cl, X, Y)
                    }
                    out
                  },
                  "MPI" = mm.Rmpi,
                  "MPI-wB" = mm.Rmpi.C,
                  "PVM" = mm.rpvm,
                  "PVM-wB" = mm.rpvm.C,
                  stop("no such type")
                  )
    return(foo)
  }
  if(taskNr==2){
    foo <- switch(type,
                  "normal" = Monte_Carlo_simulation,
                  "MPI" = mcs.Rmpi,
                  stop("no such type")
                  )
    return(foo)
  }
  stop("no such task")
}

benchmark_prepare <- function(x,n){
  type <- benchmark_type(x)
  if(any(type == c("normal", "native-BLAS", "goto-BLAS", "MKL-BLAS", "OpenMP")))
    return(n)
  if(type == "snow-MPI"){
    if(!(require("snow")&&require("Rmpi")))
      stop("Packages 'Rmpi' and 'snow' are required to run this benchmark")
    cl <- makeCluster(n, type = "MPI")
    return(n)
  }
  if(any(type == c("MPI","MPI-wB"))){
    if(!(require("Rmpi")))
      stop("Packages 'Rmpi' is required to run this benchmark")
    mpi.spawn.Rslaves(nslaves=n)
    if(pmatch(benchmark_task(x), benchmark_tasks(x)) == 2) ## Initialize parallel RNG for MCS
      mpi.setup.sprng()
    return(n)
  }
  if(type == "snow-PVM") {
    if(!(require("snow")&&require("rpvm")))
      stop("Packages 'rpvm' and 'snow' are required to run this benchmark")
    cl <- makeCluster(n, type = "PVM")
    return(cl)
  }
  if(any(type == c("PVM","PVM-wB"))) {
    if(!require("rpvm"))
      stop("Package 'rpvm' is required to run this benchmark")
    return(n)
  }
  else
    stop("No init procedure found")
}

benchmark_close <- function(x, prep){
  type <- benchmark_type(x)
  if(any(type == c("normal", "native-BLAS", "goto-BLAS", "MKL-BLAS", "OpenMP", "PVM")))
    return()
  if(type == "snow-MPI"){
    cl <- getMPIcluster()
    stopCluster(cl)
  }
  if(type == "snow-PVM"){
     stopCluster(prep)
  }
  if(any(type == c("MPI","MPI-wB")))
    mpi.close.Rslaves()
}

## benchmark workhorses

benchmark_matrix_multiplication <- function(x){
  ## build dataframe for results
  out <- benchmark_data.frame()
  
  foo <- benchmark_function_to_apply(x)
  for( n_cpu in benchmark_cpu_range(x)){
    if(n_cpu == 1) {
      for(i in 1 : benchmark_runs(x)){
        data <- benchmark_data(x)
        if(!length(data)==2)
          stop("'data' supplied must be a list containing two matrices")
        out <- rbind(out,c(benchmark_task(x),benchmark_type(x),n_cpu,
                           as.vector(system.time(foo(data[[1]],data[[2]])))[1:3],
                           benchmark_is_parallel(x), i))
      }
    }
    else {
      prep <- benchmark_prepare(x,n_cpu)
      for(i in 1 : benchmark_runs(x)){
        data <- benchmark_data(x)
        if(!length(data)==2)
          stop("'data' supplied must be a list containing two matrices")
        tmp <- c(benchmark_task(x), benchmark_type(x), n_cpu,
                 as.vector(system.time(foo(data[[1]], data[[2]], prep)))[1:3],
                 TRUE, i)
        out <- rbind(out,tmp)
      }
      benchmark_close(x, prep)     
    }
  }
  ## format data.frame accordingly
  out <- out[-1,]
  out$time_usr <- as.numeric(out$time_usr)
  out$time_sys <- as.numeric(out$time_sys)
  out$time_ela <- as.numeric(out$time_ela)
  out$is_parallel <- as.logical(out$is_parallel)
  out$n_cpu <- as.integer(out$n_cpu)
  out$run <- as.integer(out$run)
  out
}

benchmark_Monte_Carlo_simulation <- function(x){
  ## build dataframe for results
  out <- benchmark_data.frame()
  
  data <- benchmark_data(x)
  if(!length(data)==6)
    stop("'data' supplied must be a list containing six parameters")
  foo <- benchmark_function_to_apply(x)  
  for( n_cpu in benchmark_cpu_range(x)){
    if(n_cpu == 1)
      out[n_cpu,] <- c(benchmark_task(x),benchmark_type(x),n_cpu,
                   as.vector(system.time(foo(data[[1]],data[[2]],data[[3]],data[[4]],
                                             data[[5]],data[[6]])))[1:3], benchmark_is_parallel(x), i)
    else {
      prep <- benchmark_prepare(x,n_cpu)
      tmp <- c(benchmark_task(x), benchmark_type(x), n_cpu,
               as.vector(system.time(foo(data[[1]],data[[2]],data[[3]],data[[4]],
                                             data[[5]],data[[6]], prep)))[1:3], TRUE, i)
      out[n_cpu,] <- tmp
      benchmark_close(x, prep)     
    }
  }
  ## format data.frame accordingly
  out <- out[-1,]
  out$time_usr <- as.numeric(out$time_usr)
  out$time_sys <- as.numeric(out$time_sys)
  out$time_ela <- as.numeric(out$time_ela)
  out$is_parallel <- as.logical(out$is_parallel)
  out$n_cpu <- as.integer(out$n_cpu)
  out$run <- as.integer(out$run)
  out
}


## S3 generic
## generics
speedup <- function(x, ...){
  UseMethod("speedup")
}

## methods
speedup.default <- function(x){
  writeLines("Use a vector or a benchmark results object as input.")
}

speedup.numeric <- function(x){
  x[1]/x
}

speedup.benchmark_results <- function(x){
  vec <- x$time_ela
  names(vec) <- x$type
  speedup(vec)
}
## S3 methods

## print method
print.benchmark <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  run <- ifelse(benchmark_is_parallel(x),"parallel","serial")
  task <- benchmark_task(x)
  type <- benchmark_type(x)
  writeLines(paste("A", run, "benchmark running task:", task, "-", type))
}

## plot method (requires vcd)

require("vcd")

plot.benchmark_results <- function(x, task="all", ... ){
  if(!class(x)[1]=="benchmark_results")
    stop("'x' not of class 'benchmark_results'")

  ntypes <- length(unique(x$type))
  ntasks <- length(unique(x$task))

  aggr <- aggregate(subset(x,select=c("time_usr","time_sys","time_ela")), list(type=x$type,n_cpu=x$n_cpu), median)
  aggr$n_cpu <- as.integer(as.character(aggr$n_cpu))
  
  if(ntypes > 11)
    stop("more than 11 functions in a benchmark are not supported yet")

  ## define plot region, colorspace and other plot parameters
  xlim <- c(0.5,max(as.numeric(x$n_cpu),na.rm=TRUE)+0.5)
  
  ncolors <- ntypes*ntasks
  colors <- rainbow_hcl(ncolors, c=80, l=65, start = 20, end = 340)
  ##ltys <- c(1:6,1:6)
  pchs <- c(NA,21:25,21:25,21)
  par(mar=c(5,4,4,5))
  main=paste("Task:",task)

  ## plot reference
  ref <- x$type[1]
  reference=subset(aggr,type==ref)

  ## don't plot upward trend in execution time beyond a limit
  limit <- 0.75*reference$time_ela
  treshold <- max(x$n_cpu)/2
  aggr <- rbind(subset(aggr, n_cpu <= treshold ), subset(subset(aggr, n_cpu > treshold),time_ela<limit))

 
  ylim <- c(min(aggr$time_ela,na.rm=TRUE)*0.9,max(aggr$time_ela,na.rm=TRUE)*1.3)
  
  plot( x = as.numeric(reference$n_cpu), y = reference$time_ela, col=colors[1], xlim = xlim, ylim = ylim, type = "l",
       pch = pchs[1], ,xlab = "# of CPUs", ylab = "execution time [s]", main = main)
  abline(h=reference$time_ela, col=colors[1], lty = 3)
  ##plot the rest
  results.to.plot <- unique(aggr$type)
  results.to.plot <- results.to.plot[-which(results.to.plot==ref)]
  for(i in 1:length(results.to.plot)){
    lines(x = as.integer(as.character(aggr$n_cpu[which(aggr$type==results.to.plot[i])])), y = aggr$time_ela[which(aggr$type==results.to.plot[i])],
          col=colors[i+1], type = "b", lty= 1, pch = pchs[i+1])
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
  legend("topright", c(ref,as.character(results.to.plot)), col=colors[1:ntypes], lty = c(3,rep(1, length(results.to.plot))), bty = "o", box.lty = 0, bg = "white", pch = pchs[1:ntypes])
  box()
}
