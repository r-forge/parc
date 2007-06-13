
library("paRc")

## define functions

serial_mm_native <- function(X, Y) {
  X%*%Y
}

serial_mm <- function(X, Y) {
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")
  ##if(!all(dim(X)==dim(Y)))
  ##    stop("'X' and 'Y' must have the same order")
  
  dx <- dim(X) ## dimensions of matrix X
  dy <- dim(Y) ## dimensions of matrix Y
  if(!(dx[2]==dy[1]))
    stop("'X' and 'Y' not compatible")

  x <- as.vector(X)
  y <- as.vector(Y)
  z <- vector()
  length(z) <- dx[1]*dy[1]
  for(i in 1:dx[1])
    for(j in 1:dy[2]){
      sum = 0.0
      for(k in 1:dx[2])
	sum <- sum + x[i + (k-1)*dx[1]]*y[k + (j-1)*dy[1]]
      z[i + (j-1)*dx[1]] <- sum
    }
  matrix(z,ncol=dy[1])
}



parallel_mm <- function(X, Y, n_cpu = 1, spawnRslaves=FALSE) {
  ## Input validation
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")
  #if(!all(dim(X)==dim(Y)))
  #    stop("'X' and 'Y' must have the same order")
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  if(!(dx[1]==dy[2])&&(dx[2]==dy[1]))
    stop("'X' and 'Y' not compatible")

  ## uncomment when code fixed
  ##x <- as.vector(t(X)) ## because mpi.scatterv doesn't work
    
  if( n_cpu == 1 )
    return(serial_mm(X, Y))
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu - 1)

  mpi.bcast.Robj2slave(Y) ## needed on every node
  mpi.bcast.Robj2slave(X) ## not needed when code fixed

  ## FIXME: the following code has to be fixed, we want to use
  ##        mpi.scatter()
  ## send only those rows which are needed by the slaves
  ##nrow_to_send <- ceiling(dx[1]/n_cpu)
  ##nrow_to_send_master <- dx[1] - (n_cpu - 1)*nrow_to_send
  ##len_slaves <- nrow_to_send*dx[2]
  ##len_master <- nrow_to_send_master*dx[2]
  ##mpi.bcast.Robj2slave(len_slaves)
  ##mpi.bcast.cmd(local_x<-mpi.scatter(double(len_slaves), type=2,
  ##                                   rdata=double(len_slaves),root=0))
  ##local_x <- mpi.scatter(x, type=2,
  ##                       rdata=double(len_master),root=0)
  ##mpi.bcast.Robj2slave(serial_mm)
  ##mpi.bcast.cmd(local_mm <- serial_mm(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y))
  ##local_mm <- serial_mm(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y)


  mpi.bcast.cmd(commrank <- mpi.comm.rank())
  commrank <- mpi.comm.rank()
  mpi.bcast.Robj2slave(n_cpu)

  nrows_on_slaves <- ceiling(dx[1]/n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1)*nrows_on_slaves
  mpi.bcast.Robj2slave(nrows_on_slaves)
  mpi.bcast.Robj2slave(nrows_on_last)
  
  mpi.bcast.Robj2slave(serial_mm)
  mpi.bcast.cmd(if(commrank==(n_cpu - 1)) local_mm <- serial_mm(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_last),],Y) else local_mm <- serial_mm(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),],Y))
  local_mm <- serial_mm(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),],Y)
               
  mpi.bcast.cmd(mpi.gather.Robj(local_mm,root=0,comm=1))
  mm <- mpi.gather.Robj(local_mm, root=0, comm=1)
  out <- NULL
  for(i in 1:n_cpu)
    out <- rbind(out,mm[[i]])
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  out
}

## serial examples
set.seed(1782)
n <- 500
A <- matrix(runif(n*n,-5,5),ncol=n)
B <- matrix(runif(n*n,-5,5),ncol=n)
system.time(A%*%B)
system.time(c_serial_mm(A,B))

## parallel examples
library("Rmpi")
n_cpu <- 2
mpi.spawn.Rslaves(nslaves = n_cpu - 1)
mpi.comm.size() 
X<-matrix(1:16,nrow=4)
Y<-matrix(16:1,nrow=4)

serial_mm(X,Y)
parallel_mm(X,Y,n_cpu,spawnRslaves=FALSE)
X%*%Y

mpi.close.Rslaves()

## example test run serial vs. MPI implementation for 80x80 Matrix

n <- 80
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

n_cpu <- 4

mpi.spawn.Rslaves(nslaves = n_cpu - 1)

system.time(serial_mm(A,B))
system.time(parallel_mm(A,B,n_cpu))
system.time(A%*%B)

mpi.close.Rslaves()

## benchmark using a cpu range

## define Benchmark
bench <- list()
bench$functions_to_be_applied <- c("serial_mm","serial_mm_native","parallel_mm")
bench$is_parallel <- c(FALSE,FALSE,TRUE)
bench$cpu_range <- 1:4
bench$task <- "matrix multiplication"
bench$data1 <- matrix(runif(n*n,-5,5),nrow=n)
bench$data2 <- matrix(runif(n*n,-5,5),nrow=n)
class(bench) <- "benchmark"

run.benchmark(bench)

mpi.exit()


