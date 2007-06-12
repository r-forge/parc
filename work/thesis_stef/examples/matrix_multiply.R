
library("paRc")

n <- 500
A <- matrix(runif(n*n,-5,5),ncol=n)
B <- matrix(runif(n*n,-5,5),ncol=n)
system.time(A%*%B)
system.time(c_serial_mm(A,B))

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



parallel_mm <- function(X, Y, n_cpu,spawnRslaves=FALSE) {
  ## Input validation
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")
  #if(!all(dim(X)==dim(Y)))
  #    stop("'X' and 'Y' must have the same order")
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  if(!(dx[1]==dy[2])&&(dx[2]==dy[1]))
    stop("'X' and 'Y' not compatible")

  x <- as.vector(t(X)) ## because mpi.scatterv doesn't work
    
  if( n_cpu == 1 )
    serial_mm(X, Y)
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu - 1)

  mpi.bcast.Robj2slave(Y) ## needed on every node

  ## send only those rows which are needed by the slaves
  nrow_to_send <- dx[1]/n_cpu
  #mpi.bcast.Robj2slave(nrow_to_send)
  len <- length(x)/n_cpu
  mpi.bcast.Robj2slave(len)
  mpi.bcast.cmd(local_x<-mpi.scatter(double(len), type=2,
                                     rdata=double(len),root=0))
  local_x <- mpi.scatter(x, type=2,
                         rdata=double(len),root=0)

  
  mpi.bcast.Robj2slave(serial_mm)
  mpi.bcast.cmd(local_mm <- serial_mm(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y))
  local_mm <- serial_mm(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y)
  mpi.bcast.cmd(mpi.gather.Robj(local_mm,root=0,comm=1))
  mm <- mpi.gather.Robj(local_mm, root=0, comm=1)
  out <- NULL
  for(i in 1:n_cpu)
    out <- rbind(out,matrix(mm[,i],nrow=nrow_to_send))
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  out
}


## examples
library("Rmpi")
n_cpu <- 2
mpi.spawn.Rslaves(nslaves = n_cpu - 1)
mpi.comm.size() 
X<-matrix(1:16,nrow=4)
Y<-matrix(16:1,nrow=4)

serial_mm(X,Y)
parallel_mm(X,Y,2,spawnRslaves=TRUE)
X%*%Y

mpi.close.Rslaves()

n <- 80
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

n_cpu <- 4
mpi.spawn.Rslaves(nslaves = n_cpu - 1)
system.time(serial_mm(A,B))
system.time(parallel_mm(A,B,4))
system.time(A%*%B)

mpi.close.Rslaves()
mpi.exit()
