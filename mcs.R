library("paRc")
library("Rmpi")

myopt <- list()
myopt$mu <- 0.1           ## expectation of underlying
myopt$sigma <- 0.4        ## standard deviation of underlying
myopt$type <- "Call"      ## type of the option
myopt$strikeprice <- 100  ## strikeprice of the option
myopt$present <- 100      ## present value of the underlying
myopt$maturity <- 1/12    ## time to maturity (in years)

myopt <- as.option(myopt)   ## coercing 'list' to 'option'
mpi.spawn.Rslaves(nslaves=2)
mcs.Rmpi(myopt,0.1,30,5000,n_cpu=2,spawn=F,debug=TRUE)
mpi.close.Rslaves()
mcs.Rmpi(myopt,0.1,30,5000,n_cpu=2,spawn=T,debug=TRUE)
system.time(mcs.Rmpi(myopt,0.1,30,5000,n_cpu=1,debug=TRUE))
system.time(mcs.Rmpi(myopt,0.1,30,5000,n_cpu=3,spawn=T,debug=TRUE))

