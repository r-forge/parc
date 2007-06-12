## Test script for running Rmpi on cluster
## boot lam with 4 or more CPU's
## theussl, 2007

library("Rmpi")

## how many nodes are available
mpi.universe.size()

## spawn two slaves
n_cpu <- 30
mpi.spawn.Rslaves(nslaves = n_cpu - 1)

## how many cpu's are in the communicator
mpi.comm.size()

## which hosts are in the communicator?
mpi.remote.exec(mpi.get.processor.name())
mpi.get.processor.name()

## shutdown Rslaves and Rmpi
mpi.close.Rslaves()
mpi.exit()

