##########################################################
## R Package: paRc
## matrix_multiply.R
## application: Parallel Matrix Multiplication
## theussl, 2007
##########################################################

## input validation function
matrix.mult.validate <- function(X, Y, dx, dy){
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")
  
  if(!((dx[1]==dy[2])&&(dx[2]==dy[1])))
    stop("'X' and 'Y' not compatible")
}

## serial version of matrix multiplication
serial.matrix.mult <- function(X, Y)
{
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix.mult.validate(X, Y, dx, dy)
  ## data preparation
  x <- as.vector(X)
  storage.mode(x) <- "double"
  y <- as.vector(Y)
  storage.mode(y) <- "double"
  ## C call of serial matrix multiplication    
  out <- .C("Serial_matrix_mult",
            x, as.integer(dx[1]), as.integer(dx[2]),
            y, as.integer(dy[1]), as.integer(dy[2]),
            z = double(dx[1]*dy[2]),
            PACKAGE = "paRc")
  matrix(out$z,ncol=dy[2])
}


## parallel version of matrix multiplication
omp.matrix.mult <- function(X, Y, n_cpu = 1)
{
  
  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  ## input validation
  matrix.mult.validate(X, Y, dx, dy)
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
            z = double(dx[1]*dy[2]),
            PACKAGE = "paRc")
  matrix(out$z,ncol=dy[2])
}


serial.dot.product <- function(x, y)
{
    if(!is.vector(x) && !is.vector(y))
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
