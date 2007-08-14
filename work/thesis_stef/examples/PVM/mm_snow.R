
## load required libraries
#library("snow")
#library("paRc")
library("snow")

#cl <- makeCluster(4, type="PVM")

## some information about the parallel environment
#cl
#.PVM.config()

writeLines("test test test")

options <- defaultClusterOptions
getClusterOption("scriptdir", options)
getClusterOption("rprog", options)
getClusterOption("homogeneous")

n <- 1000
A <- matrix(runif(n*n,-5,5),nrow=n)
B <- matrix(runif(n*n,-5,5),nrow=n)

x <- A%*%B

#X <- parMM(cl, A, B)

dim(x)

system.time(A%*%B)

#stopCluster(cl)


