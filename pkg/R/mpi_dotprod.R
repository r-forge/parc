## Examples from 'Parallel Programming with MPI' from Peter S. Pacheco
## implemented in R, Theussl 2007
## chapter 5.5, p. 75: Dot Product

## this can be done more efficient, it's just for trying

library("Rmpi")

serial_dot <- function(x, y) {
  sum <- 0
  n <- length(x)
  if( length(y) != n)
    stop("'y' must have the same length as 'x'")
  for( i in 1:n)
    sum <- sum(sum, x[i]*y[i])
  sum
}
  
parallel_dot <- function(x, y, n_cpu,spawnRslaves=FALSE) {
  if( n_cpu == 1 )
    serial_dot(x, y)
  if( spawnRslaves == TRUE )
    mpi.spawn.Rslaves(nslaves = n_cpu - 1)
  len <- length(x)/n_cpu
  mpi.bcast.Robj2slave (len)
  mpi.bcast.cmd(local_x<-mpi.scatter(double(len),type=2,rdata=double(len),root=0))
  local_x <- mpi.scatter(x,type=2,rdata=double(len),root=0)
  mpi.bcast.cmd(local_y<-mpi.scatter(double(len),type=2,rdata=double(len),root=0))
  local_y <- mpi.scatter(y,type=2,rdata=double(len),root=0)
  mpi.bcast.Robj2slave(serial_dot)
  mpi.bcast.cmd(local_dot <- serial_dot(local_x,local_y))
  local_dot <- serial_dot(local_x,local_y)
  mpi.bcast.cmd(mpi.reduce(local_dot, type=2, op="sum",dest=0,comm=1))
  dot <- mpi.reduce(local_dot,type=2,op="sum",dest=0,comm=1)
  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  dot
}

## example
x <- runif(100000,-3,3)
y <- runif(100000,-2,4)

system.time(x%*%y)           ## fast, faster, fastest
system.time(serial_dot(x,y)) ## very slow
system.time(parallel_dot(x,y,4,spawnRslaves = TRUE)) ## MPI version with spawn of 3 Rslaves
mpi.spawn.Rslaves(nslaves=3)
system.time(parallel_dot(x,y,4,spawnRslaves = FALSE)) ## without spawn time
mpi.close.Rslaves()


