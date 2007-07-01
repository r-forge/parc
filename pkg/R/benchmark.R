## FIXME: we need a reference in a benchmark: most of the time serial version
## defined in benchmark object -> the reference in a task is in the first line


## extractor functions

benchmark.task <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$task
}  

benchmark.functions.to.apply <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$functions_to_be_applied
}  

## TODO: generic
benchmark.is.parallel <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  out <- x$is_parallel
  names(out) <- x$functions_to_be_applied
  out
}

benchmark.cpu.range <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$cpu_range
}

benchmark.avail.cpu <- function(x) {
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  x$avail_cpu
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
  type <- ifelse(any(benchmark.is.parallel(x))==TRUE,"parallel","serial")
  task <- benchmark.task(x)
  writeLines(paste("A", type, "benchmark running task:", task))
}

## main benchmark function
run.benchmark <- function(x){
  ## input validation
  if(class(x)!="benchmark")
    stop("'x' not of class 'benchmark'")
  if(!is.integer(benchmark.cpu.range(x)))
    stop("all values in 'cpu_range' must be of type integer")
  if(!all(benchmark.cpu.range(x) > 0))
    stop("all values in 'cpu_range' must be greater than 0")

  task <- benchmark.task(x)
  tasks <- c(
             "matrix_multiplication",
             "task2"
             )

  if(is.null(task)) stop("No task chosen to benchmark")
  else taskNr <- pmatch(tolower(task), tolower(tasks))
  if(is.na(taskNr)) stop (paste("Unknown task:",sQuote(task)))

  if(methodNr == 1) {
    results <- bm.matrix.multiplication(x)
  }else if(methodNr == 2) {
    writeLines("Not implemented yet")
  }

  class(results) <- c("bench_results",class(results))
  results
}


bm.matrix.multiplication <- function(x){

  ## build dataframe for results
  out <- data.frame(task=NA,foo=NA,n_cpu=NA,time_usr=NA,time_sys=NA,time_ela=NA,
                    is_parallel=NA)

  foocount <- length(benchmark.functions.to.apply(x))
  data <- benchmark.data(x)
  for( n_cpu in benchmark.cpu.range(x)){
    if(n_cpu == 1){
      for(i in 1:foocount){
        foo <- match.fun(benchmark.functions.to.apply(x)[i])
        out[i,] <- c(benchmark.task(x),benchmark.functions.to.apply(x)[i],n_cpu,
                     as.vector(system.time(foo(data[[1]],data[[2]])))[1:3],
                     benchmark.is.parallel(x)[i])
      }
    }
    else {
      parfoos <- benchmark.functions.to.apply(x)[benchmark.is.parallel(x)]
      avail_cpu <- benchmark.avail.cpu(x)[benchmark.is.parallel(x)]
      for(i in 1:length(parfoos)){
        ##mpi.spawn.Rslaves(nslaves = n_cpu - 1)
        if(n_cpu > avail_cpu[i])
          tmp <- c(benchmark.task(x),parfoos[i],n_cpu,
                   c(NA,NA,NA),TRUE)
        else {
          foo <- match.fun(parfoos[i])        
          tmp <- c(benchmark.task(x),parfoos[i],n_cpu,
                   as.vector(system.time(foo(data[[1]], data[[2]],n_cpu)))[1:3],
                   TRUE)
        }
        out[foocount+n_cpu-2+i,] <- tmp
                                      
        ##mpi.close.Rslaves()
      }
    }
  }
}
