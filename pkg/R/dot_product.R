
c_serial_dot <- function(x, y)
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
