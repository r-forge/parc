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

## replacement functions


## print method
print.benchmark <- function(x){
  if(class(x) != "benchmark")
    stop("'x' not of class 'benchmark'")
  type <- ifelse(any(benchmark.is.parallel(x))==TRUE,"parallel","serial")
  task <- task_in_benchmark(x)
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
  if(max(benchmark.cpu.range(x)) > mpi.universe.size())
    stop("all values in 'cpu_range' must be smaller than the number of available CPU's oin the LAM/MPI environment")

  ## build dataframe for results
  out <- data.frame(task=NA,foo=NA,n_cpu=NA,time_usr=NA,time_sys=NA,time_ela=NA,is_parallel=NA)
  foocount <- length(benchmark.functions.to.apply(x))
  for( n_cpu in benchmark.cpu.range(x)){
    if(n_cpu == 1){
      for(i in 1:foocount){
        foo <- match.fun(benchmark.functions.to.apply(x)[i])
        out[i,] <- c(benchmark.task(x),benchmark.functions.to.apply(x)[i],n_cpu,as.vector(system.time(foo(x$data1,x$data2)))[1:3],benchmark.is.parallel(x)[i])
      }
    }
    else {
      parfoos <- benchmark.functions.to.apply(x)[benchmark.is.parallel(x)]
      for(i in 1:length(parfoos)){
        mpi.spawn.Rslaves(nslaves = n_cpu - 1)
        foo <- match.fun(parfoos[i])
        out[foocount+n_cpu-2+i,] <- c(benchmark.task(x),parfoos[i],n_cpu,as.vector(system.time(foo(x$data1,x$data2,n_cpu)))[1:3],TRUE)
        mpi.close.Rslaves()
      }
    }
  }
  class(out) <- c("bench_results","data.frame")
  out
}
