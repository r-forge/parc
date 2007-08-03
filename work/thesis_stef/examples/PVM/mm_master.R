##########################################################
## script file: PVM matrix multiplication
## mm_master.R
## application: Parallel Matrix Multiplication
## theussl, 2007
##########################################################

require("paRc")
require("Rmpi")





## master job
pvm.matrix.mult <- function(X, Y, n_cpu = 1, spawnRslaves=TRUE) {
  ## Input validation
  if(!(is.matrix(X) && is.matrix(Y)))
    stop("'X' and 'Y' must be matrices.")

  dx <- dim(X) ## dimensions of matrix A
  dy <- dim(Y) ## dimensions of matrix B
  if(!(dx[2]==dy[1]))
    stop("'X' and 'Y' not compatible")

  ## uncomment when code fixed
  ##x <- as.vector(t(X)) ## because mpi.scatterv doesn't work

  WORKTAG <- 17
  RESULTAG <- 82
  
  if( n_cpu == 1 )
    return(serial.matrix.mult(X, Y))

  mytid <- .PVM.mytid()
  children <- .PVM.spawnR(ntask = NTASK, slave = "mm_slave")
  if (all(children < 0)) {
    cat("Failed to spawn any task: ", children, "\n")
    .PVM.exit()
  }
  else if (any(children < 0)) {
    cat("Failed to spawn some tasks.  Successfully spawned ",
        sum(children > 0), "tasks\n")
    children <- children[children > 0]
  }

  nrows_on_slaves <- ceiling(dx[1]/n_cpu)
  nrows_on_last <- dx[1] - (n_cpu - 1)*nrows_on_slaves

  # distribute data
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
  
  commrank <- mpi.comm.rank()
  local_mm <- serial.matrix.mult(X[(nrows_on_slaves*commrank + 1):(nrows_on_slaves*commrank + nrows_on_slaves),],Y)
  mm <- mpi.gather.Robj(local_mm, root=0, comm=1)

  partial.results <- list()
  for (child in children) {
    .PVM.recv(-1, RESULTAG)
    rank <- .PVM.upkint()
    partial.results[[order]] <- .PVM.upkdblvec()
  }
  .PVM.exit()
  return(unlist(partial.results))
 }