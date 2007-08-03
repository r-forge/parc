##########################################################
## script file: slave R script for PVM matrix mult
## matrix_multiply.R
## application: Parallel Matrix Multiplication
## theussl, 2007
##########################################################

require("paRc")
require("rpvm")

WORKTAG <- 17
RESULTAG <- 82

#mytid  <- .PVM.mytid ()
myparent  <- .PVM.parent ()

## Receive work from parent (a matrix)
buf <- .PVM.recv (myparent, WORKTAG)
rank <- .PVM.upkint()
n_cpu <- .PVM.upkint()
nrows_on_slaves <- .PVM.upkint()
nrows_on_last <- .PVM.upkint()
X <- .PVM.upkdblmat()
Y <- .PVM.upkdblmat()

if(order==(n_cpu - 1))
  local_mm <- serial.matrix.mult(X[(nrows_on_slaves*rank + 1):(nrows_on_slaves*rank + nrows_on_last),],Y)
else
  local_mm <- serial.matrix.mult(X[(nrows_on_slaves*rank + 1):(nrows_on_slaves*rank + nrows_on_slaves),],Y)

## Send result back
.PVM.initsend()
.PVM.pkint(rank)
.PVM.pkdblmat(local_mm)
.PVM.send (myparent, RESULTAG)

## Exit PVM
.PVM.exit ()
## Exit R
q (save="no")
