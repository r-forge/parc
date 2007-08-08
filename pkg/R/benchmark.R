## FIXME: we need a reference in a benchmark: most of the time serial version
## defined in benchmark object -> the reference in a task is in the first line

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



## extractor functions

benchmark.task <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks[x$task]
}

bm.tasks <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$tasks
}

benchmark.type <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$types[x$type]
}


## TODO: generic
benchmark.is.parallel <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$is_parallel
}

benchmark.cpu.range <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$cpu_range
}

benchmark.data <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$data
}

## replacement functions


## print method
print.benchmark <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  run <- ifelse(benchmark.is.parallel(x),"parallel","serial")
  task <- benchmark.task(x)
  type <- benchmark.type(x)
  writeLines(paste("A", run, "benchmark running task:", task, "-", type))
}

## main benchmark function
run.benchmark <- function(x){
  ## input validation
  if(class(x)!="benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(benchmark.task(x), bm.tasks(x))

  if(taskNr == 1) {
    results <- bm.matrix.multiplication(x)
  }else if(taskNr == 2) {
    writeLines("Not implemented yet")
    results <- NULL
  }

  ## format data.frame accordingly
  results$time_usr <- as.numeric(results$time_usr)
  results$time_sys <- as.numeric(results$time_sys)
  results$time_ela <- as.numeric(results$time_ela)
  results$is_parallel <- as.logical(results$is_parallel)
  class(results) <- c("bench_results",class(results))
  results
}

bm.data.frame <- function(){
  data.frame(task=NA, type=NA, n_cpu=NA, time_usr=NA, time_sys=NA,
                    time_ela=NA, is_parallel=NA)
}


bm.function.to.apply <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")

  taskNr <- pmatch(benchmark.task(x), bm.tasks(x))
  type <- benchmark.type(x)
  
  if(taskNr==1)
    foo <- switch(type,
                  "normal" = function(X,Y){X%*%Y},
                  stop("no such type")
                  )
  else
    stop("no such task")
  foo
}  

bm.matrix.multiplication <- function(x){
  ## build dataframe for results
  out <- bm.data.frame()
  
  data <- benchmark.data(x)
  if(!length(data)==2)
    stop("'data' supplied must be a list containing two matrices")
  foo <- bm.function.to.apply(x)  
  for( n_cpu in benchmark.cpu.range(x)){
    if(n_cpu == 1)
      out[n_cpu,] <- c(benchmark.task(x),benchmark.type(x),n_cpu,
                   as.vector(system.time(foo(data[[1]],data[[2]])))[1:3],
                   benchmark.is.parallel(x))
    else {
      ##mpi.spawn.Rslaves(nslaves = n_cpu - 1)
      tmp <- c(benchmark.task(x), benchmark.type(x), n_cpu,
               as.vector(system.time(foo(data[[1]], data[[2]], n_cpu)))[1:3],
               TRUE)
      out[n_cpu] <- tmp
      
      ##mpi.close.Rslaves()
    }
  }
  out
}
