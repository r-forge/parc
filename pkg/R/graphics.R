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

plot.bench_results <- function(x, task="all", ... ){
  if(!class(x)[1]=="bench_results")
    stop("'x' not of class 'bench_results'")

  count.foo <- length(unique(x$foo))
  count.task <- length(unique(x$task))

  if(count.foo > 5)
    stop("more than 5 functions in a benchmark are not supported yet")

  
  ## define plot region, colorspace and other plot parameters
  xlim <- c(0,max(as.numeric(x$n_cpu))+1)
  ylim <- c(0,max(x$time_ela)+1)
  ncolors <- count.foo*count.task
  colors <- rainbow_hcl(ncolors, c=80, l=65, start = 20, end = 340)
  ltys <- c(1:6)
  pchs <- c(21:25)
  par(mar=c(5,4,4,5))
  main=paste("Task:",task)

  ## plot reference
  ref <- x$foo[1]
  plot( x = as.numeric(x$n_cpu)[1], y = x$time_ela[1], col=colors[1], xlim = xlim, ylim = ylim, type = "b",
       pch = pchs[1], ,xlab = "# of CPUs", ylab = "execution time", main = main)

  ##plot the rest
  results.to.plot <- unique(x$foo)
  results.to.plot <- results.to.plot[-which(results.to.plot==ref)]
  for(i in 1:length(results.to.plot)){
    lines(x = as.numeric(x$n_cpu[which(x$foo==results.to.plot[i])]), y = x$time_ela[which(x$foo==results.to.plot[i])],
          col=colors[i+1], type = "b", lty= ltys[i+1], pch = pchs[i+1])
  }

  ## plot speedup
  ##par(new = TRUE)
  ##xlim <- c(0,x$cpucount[x$cpusteps]+1)
  ##ylim <- c(0,max(x$speedup))
  ##plot(x$cpucount, x$speedup[1,], col = colors[1+x$kinds], axes = FALSE, xlim = xlim, ylim=ylim, type = "b", xlab = "", ylab = "", lty=ltys[1], pch = pchs[1])
  ##for(i in 2:x$kinds)
  ##  lines(x = x$cpucount, y = x$speedup[i,], col=colors[i+x$kinds], type = "b", lty= ltys[i], pch = pchs[i])
  ##axis(4)
  ##mtext("Speedup", side = 4, line = 3)

  ## legend
  legend("topleft", c(ref,results.to.plot), col=colors[1:count.foo], lty = ltys[1:count.foo], bty = "n", pch = pchs[1:count.foo])
}


