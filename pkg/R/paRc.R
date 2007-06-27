omp_test <- function(){
  .C("OMP_test", PACKAGE = "paRc")
}
