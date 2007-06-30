##########################################################
## script file: miscellaneous function definitions 
## other_code_junks.R
## theussl, 2007
##########################################################


## serial matrix multiplication using R loops
serial.matrix.mult.Rloop <- function(X, Y) {
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")
  
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

