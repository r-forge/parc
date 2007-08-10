##########################################################
## script file: function definitions
## matrix_multiply.R
## application: Parallel Matrix Multiplication
## theussl, 2007
##########################################################

require("paRc")
require("Rmpi")

serial.matrix.mult.native <- function(X, Y) {
  X%*%Y
}

## slave job
mpi.matrix.mult.slave <- function(){
  #require("paRc")
  commrank <- mpi.comm.rank() -1
  if(commrank==(n_cpu - 1))
  #  local_mm <- serial.matrix.mult(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_last),],Y)
    local_mm <- X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_last),]%*%Y
  else
#    local_mm <- serial.matrix.mult(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),],Y)
    local_mm <- X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),]%*%Y
  mpi.gather.Robj(local_mm,root=0,comm=1)    
}

## master job
mpi.matrix.mult <- function(X, Y, n_cpu = 1, spawnRslaves=TRUE) {
  ## Input validation
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")

  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  if(!(dx[2]==dy[1]))
    stop("'X' and 'Y' not compatible")

  ## uncomment when code fixed
  ##x <- as.vector(t(X)) ## because mpi.scatterv doesn't work
    
  if( n_cpu == 1 )
    return(serial.matrix.mult(X, Y))
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu)

  mpi.bcast.Robj2slave(Y) ## needed on every node
  mpi.bcast.Robj2slave(X) ## not needed when code fixed
  mpi.bcast.Robj2slave(n_cpu)
  
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
  ##mpi.bcast.cmd(local_mm <- serial.matrix.mult(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y))
  ##local_mm <- serial.matrix.mult(matrix(local_x,ncol=ncol(Y),byrow=TRUE),Y)

  nrows_on_slaves <- ceiling(dx[1]/n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1)*nrows_on_slaves
  mpi.bcast.Robj2slave(nrows_on_slaves)
  mpi.bcast.Robj2slave(nrows_on_last)
  mpi.bcast.Robj2slave(mpi.matrix.mult.slave)

  mpi.bcast.cmd(mpi.matrix.mult.slave())
  
#  commrank <- mpi.comm.rank()
 # local_mm <- serial.matrix.mult(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),],Y)
  local_mm <- NULL
  mm <- mpi.gather.Robj(local_mm, root=0, comm=1)
  out <- NULL
  ## Rmpi returns a list when the vectors have different length and a matrix otherwise
  ## so we have to hack this by hand again
  #if(nrows_on_slaves == nrows_on_last)
  #  for(i in 1:n_cpu)
  #    out <- rbind(out,matrix(mm[,i],nrow=nrows_on_slaves))
  #else {
  for(i in 1:n_cpu)
    out <- rbind(out,mm[[i+1]])
  #}
  
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  out
}
