
c_serial_mm <- function(X, Y)
{
    if(!(is.matrix(X) && is.matrix(Y)))
        stop("'X' and 'Y' must be matrices.")
    ##if(!all(dim(A)==dim(B)))
    ##    stop("'A' and 'B' must be have the same order")

    dx <- dim(X) ## dimensions of matrix A
    dy <- dim(Y) ## dimensions of matrix B
    if(!(dx[1]==dy[2])&&(dx[2]==dy[1]))
      stop("'X' and 'Y' not compatible")

    x <- as.vector(X)
    storage.mode(x) <- "double"
    y <- as.vector(Y)
    storage.mode(y) <- "double"
    
    out <- .C("Serial_matrix_mult",
              x, as.integer(dx[1]), as.integer(dx[2]),
              y, as.integer(dy[1]), as.integer(dy[2]),
              z = double(dx[1]*dy[1]),
              PACKAGE = "paRc")
    matrix(out$z,ncol=dy[1])
}
