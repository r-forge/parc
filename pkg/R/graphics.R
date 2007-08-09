## graphics.R -- package paRc
## Stefan Theussl

require("vcd")

## palette code from zeileis, hornik (hcl-based color palletes
## pal <- function(col, border = "light gray", ...) {
## n <- length(col)
## plot(0, 0, type="n", xlim = c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="", ...)
## rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
## }
## This palettes are good
## pal(rainbow_hcl(10, c=80, l=65, start = 0, end = 360), main = "dynamic_bt")
## pal(rainbow_hcl(10, c=80, l=65, start = 20, end = 340), main = "dynamic_bt")

## for testing purposes
#x <- list()
#x$times <- matrix(c(65,38,20,12,7,4,3,2,1,66,33,17,9,5,3,2,1,0.6,60,40,30,18,8.9,5,5.5,3,1.6),byrow=TRUE,nrow=3)
#x$cpucount <- c(1:9)
#x$cpusteps <- 9
#x$speedup <- matrix(c(1.000000,1.710526,3.250000,5.416667,9.285714,16.250000,21.666667,32.500000,65.000000,  1.000000, 2.000000, 3.882353, 7.333333, 13.200000, 22.000000, 33.000000, 66.000000, 110.000000, 1.000000, 1.500000, 2.000000, 3.333333, 6.741573, 12.000000, 10.909091
#, 20.000000, 37.500000),byrow = TRUE, nrow = 3)
#x$kinds <- 3
#class(x) <- "cpubench"

## FIXME: pch and lty (what if we have more than 3 functions to test => rep? )
## TODO: finish legend
## TODO: plot be task: ie. task = "all", task = "matrix multiply"
## TODO: speedup plot: speedup=TRUE,FALSE



