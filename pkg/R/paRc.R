## OpenMP function wrappers

## print number of available processors in the current team
omp.get.num.procs <- function(){
  out <- .C("OMP_get_num_procs", PACKAGE = "paRc")
  out[[1]]
}

omp.get.max.threads <- function(){
  out <- .C("OMP_get_max_threads", PACKAGE = "paRc")
  out[[1]]
}

omp.set.num.threads <- function(x){
  max.procs <- omp_get_num_procs()
  if(x < 1)
    stop("'x' must be greater than or equal 1")
  if(x > max.procs)
    warning("'x' is greater than the maximum number of available processors!")
  .C("OMP_set_num_threads", as.integer(x), PACKAGE = "paRc")
  invisible(x)
}
  

omp_test <- function(){
  .C("OMP_test", PACKAGE = "paRc")
            
}


## Approximate Pi calculation
omp_calc_pi <- function(n){
  out <- .C("OMP_calc_pi", as.integer(n), pi=double(1), PACKAGE = "paRc")
  out$pi
}

serial_calc_pi <- function(n){
  out <- .C("serial_calc_pi", as.integer(n), pi=double(1), PACKAGE = "paRc")
  out$pi
}
