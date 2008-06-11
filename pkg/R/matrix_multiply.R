##########################################################
## R Package: paRc
## matrix_multiply.R
## Application: Serial and shared as well as distributed
## memory parallel matrix multiplication
## theussl, 2007, 2008
##########################################################

## input validation function
matrix_mult_validate <- function(X, Y, dx, dy){
  if( ! (is.matrix(X) && is.matrix(Y)) )
    stop( "'X' and 'Y' must be matrices." )
  
  if( ! (dx[2] == dy[1]) )
    stop( "'X' and 'Y' not compatible" )
}

## serial version of matrix multiplication implemented in C (O(n^3))
serial_matrix_multiplication <- function(X, Y)
{
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix_mult_validate(X, Y, dx, dy)
  ## data preparation
  x <- as.vector(X)
  storage.mode(x) <- "double"
  y <- as.vector(Y)
  storage.mode(y) <- "double"
  ## C call of serial matrix multiplication    
  out <- .C("Serial_matrix_mult",
            x, as.integer(dx[1]), as.integer(dx[2]),
            y, as.integer(dy[1]), as.integer(dy[2]),
            z = double(dx[1] * dy[2]),
            PACKAGE = "paRc")
  matrix(out$z, ncol = dy[2])
}

## matrix multiplication, partially using dgemm from the BLAS, i.e. for
## the inner most loop (the dot product of row and column vevtors)
matrix_multiplication_BLAS <- function(X, Y, n_cpu = 1)
{
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix_mult_validate(X, Y, dx, dy)
  ## data preparation
  ## we need the transposed matrix of X to allow a simple usage of
  ## dgemm. we need rows of X accessible in a sequence in an array.
  x <- as.vector(t(X))
  storage.mode(x) <- "double"
  y <- as.vector(Y)
  storage.mode(y) <- "double"
  out <- .C("matrix_mult_BLAS",
            x, as.integer(dx[1]), as.integer(dx[2]),
            y, as.integer(dy[1]), as.integer(dy[2]),
            z = double(dx[1] * dy[2]),
            PACKAGE = "paRc")
  matrix(out$z, ncol = dy[2])
}


## parallel version of matrix multiplication
omp_matrix_multiplication <- function(X, Y, n_cpu = 1)
{
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix_mult_validate(X, Y, dx, dy)
  ## set number of cpus to use
  omp_set_num_threads(n_cpu)
  ## data preparation
  x <- as.vector(X)
  storage.mode(x) <- "double"
  y <- as.vector(Y)
  storage.mode(y) <- "double"
  ## C call of parallel matrix multiply using openMP  
  out <- .C("OMP_matrix_mult",
            x, as.integer(dx[1]), as.integer(dx[2]),
            y, as.integer(dy[1]), as.integer(dy[2]),
            z = double(dx[1] * dy[2]),
            PACKAGE = "paRc")
  matrix(out$z, ncol = dy[2])
}

omp_matrix_multiplication_BLAS <- function(X, Y, n_cpu = 1)
{
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix_mult_validate(X, Y, dx, dy)
  ## set number of cpus to use
  omp_set_num_threads(n_cpu)
  ## data preparation
    ## we need the transposed matrix of X to allow a simple usage of
  ## dgemm. we need rows of X accessible in a sequence in an array.
  x <- as.vector(t(X))
  storage.mode(x) <- "double"
  y <- as.vector(Y)
  storage.mode(y) <- "double"
  ## C call of parallel matrix multiply using openMP  
  out <- .C("OMP_matrix_mult_BLAS",
            x, as.integer(dx[1]), as.integer(dx[2]),
            y, as.integer(dy[1]), as.integer(dy[2]),
            z = double(dx[1] * dy[2]),
            PACKAGE = "paRc")
  matrix(out$z, ncol = dy[2])
}


mm.Rmpi.slave <- function(){
  commrank <- mpi.comm.rank() - 1
  if( commrank == (n_cpu - 1) )
    local_mm <- X[(nrows_on_slaves * commrank + 1):
                  (nrows_on_slaves * commrank + nrows_on_last),] %*% Y
  else
    local_mm <- X[(nrows_on_slaves * commrank + 1):
                  (nrows_on_slaves * commrank + nrows_on_slaves),] %*% Y
  mpi.gather.Robj(local_mm, root = 0, comm = 1)    
}

mm.Rmpi.slave.C <- function(){
  require("paRc")
  commrank <- mpi.comm.rank() - 1
  if( commrank == (n_cpu - 1) )
    local_mm <- serial_matrix_multiplication(
                  X[(nrows_on_slaves * commrank + 1):
                    (nrows_on_slaves * commrank + nrows_on_last), ], Y)
  else
    local_mm <- serial_matrix_multiplication(
                  X[(nrows_on_slaves * commrank + 1):
                    (nrows_on_slaves * commrank + nrows_on_slaves), ], Y)
  mpi.gather.Robj(local_mm, root = 0, comm = 1)    
}

## master job
mm.Rmpi <- function(X, Y, n_cpu = 1, spawnRslaves = FALSE) {
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## Input validation
  matrix_mult_validate(X, Y, dx, dy)
  
  if( n_cpu == 1 )
    return(X%*%Y)
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu)

  ## broadcast data and functions necessary on slaves
  mpi.bcast.Robj2slave(Y) 
  mpi.bcast.Robj2slave(X) 
  mpi.bcast.Robj2slave(n_cpu)
  
  nrows_on_slaves <- ceiling(dx[1] / n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1) * nrows_on_slaves
  mpi.bcast.Robj2slave(nrows_on_slaves)
  mpi.bcast.Robj2slave(nrows_on_last)
  mpi.bcast.Robj2slave(mm.Rmpi.slave)

  ## start partial matrix multiplication on slaves
  mpi.bcast.cmd(mm.Rmpi.slave())

  ## gather partial results from slaves
  local_mm <- NULL
  mm <- mpi.gather.Robj(local_mm, root = 0, comm = 1)
  out <- NULL

  ## Rmpi returns a list when the vectors have different length
  ## (local_mm = NULL)
  for(i in 1:n_cpu)
    out <- rbind(out, mm[[i+1]])
  
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  out
}

mm.Rmpi.C <- function(X, Y, n_cpu = 1, spawnRslaves = FALSE) {
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## Input validation
  matrix_mult_validate(X, Y, dx, dy)
  
  if( n_cpu == 1 )
    return(serial_matrix_multiplication(X,Y))
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu)

  ## broadcast data and functions necessary on slaves
  mpi.bcast.Robj2slave(Y) 
  mpi.bcast.Robj2slave(X) 
  mpi.bcast.Robj2slave(n_cpu)
  
  nrows_on_slaves <- ceiling(dx[1] / n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1) * nrows_on_slaves
  mpi.bcast.Robj2slave(nrows_on_slaves)
  mpi.bcast.Robj2slave(nrows_on_last)
  mpi.bcast.Robj2slave(mm.Rmpi.slave.C)

  ## start partial matrix multiplication on slaves
  mpi.bcast.cmd(mm.Rmpi.slave.C())

  ## gather partial results from slaves
  local_mm <- NULL
  mm <- mpi.gather.Robj(local_mm, root = 0, comm = 1)
  out <- NULL

  ## Rmpi returns a list when the vectors have different length
  ## (local_mm = NULL)
  for(i in 1:n_cpu)
    out <- rbind(out, mm[[i+1]])
  
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  out
}

mm.rpvm <- function(X, Y, n_cpu = 1) {

  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B

  ## Input validation
  matrix_mult_validate(X,Y,dx,dy)

  ## Tags for message sending
  WORKTAG <- 17
  RESULTAG <- 82
  
  if( n_cpu == 1 )
    return(X%*%Y)

  mytid <- .PVM.mytid()
  children <- .PVM.spawnR(ntask = n_cpu, slave = "mm_slave.R")
  if ( all(children < 0) ) {
    cat("Failed to spawn any task: ", children, "\n")
    .PVM.exit()
  }
  else if ( any(children < 0) ) {
    cat("Failed to spawn some tasks.  Successfully spawned ",
        sum(children > 0), "tasks\n")
    children <- children[children > 0]
  }

  nrows_on_slaves <- ceiling(dx[1] / n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1) * nrows_on_slaves

  ## distribute data
  for(id in 1:length(children)) {
    .PVM.initsend()
    .PVM.pkint(id)
    .PVM.pkint(n_cpu)
    .PVM.pkint(nrows_on_slaves)
    .PVM.pkint(nrows_on_last)
    .PVM.pkdblmat(X)
    .PVM.pkdblmat(Y)
    .PVM.send(children[id], WORKTAG)
  }

  ## receive partial results
  partial.results <- list()
  for (child in children) {
    .PVM.recv(-1, RESULTAG)
    rank <- .PVM.upkint()
    partial.results[[rank]] <- .PVM.upkdblmat()
  }
  .PVM.exit()
  ## return in matrix form
  out <- NULL
  for(i in 1:n_cpu)
    out <- rbind(out, partial.results[[i]])
  out
}

mm.rpvm.C <- function(X, Y, n_cpu = 1) {

  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B

  ## Input validation
  matrix_mult_validate(X, Y, dx, dy)

  ## Tags for message sending
  WORKTAG <- 17
  RESULTAG <- 82
  
  if(n_cpu == 1)
    return(serial_matrix_multiplication(X, Y))

  mytid <- .PVM.mytid()
  children <- .PVM.spawnR(ntask = n_cpu, slave = "mm_slaveC.R")
  if ( all(children < 0) ) {
    cat("Failed to spawn any task: ", children, "\n")
    .PVM.exit()
  }
  else if ( any(children < 0) ) {
    cat("Failed to spawn some tasks.  Successfully spawned ",
        sum(children > 0), "tasks\n")
    children <- children[children > 0]
  }

  nrows_on_slaves <- ceiling(dx[1] / n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1) * nrows_on_slaves

  ## distribute data
  for (id in 1:length(children)) {
    .PVM.initsend()
    .PVM.pkint(id)
    .PVM.pkint(n_cpu)
    .PVM.pkint(nrows_on_slaves)
    .PVM.pkint(nrows_on_last)
    .PVM.pkdblmat(X)
    .PVM.pkdblmat(Y)
    .PVM.send(children[id], WORKTAG)
  }
  
  ## receive partial results
  partial.results <- list()
  for(child in children) {
    .PVM.recv(-1, RESULTAG)
    rank <- .PVM.upkint()
    partial.results[[rank]] <- .PVM.upkdblmat()
  }
  .PVM.exit()
  ## return in matrix form
  out <- NULL
  for(i in 1:n_cpu)
    out <- rbind(out, partial.results[[i]])
  out
}

serial_dot_product <- function(x, y)
{
    if( ! is.vector(x) && ! is.vector(y) )
        stop("'x' and 'y' must be vectors.")

    nx <- length(x)
    ny <- length(y)
    if(nx != ny)
        stop("x and y must be of the same length.")
    
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    
    out <- .C("Serial_dot", x, y, nx, prod = double(1),
              PACKAGE = "paRc")
    out$prod
}
