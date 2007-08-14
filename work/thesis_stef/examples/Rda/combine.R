## combines all data to one data file


for(n in c(1000,2500,5000){  
  
  load(paste("bmres_shared-bignode-",n,".Rda",sep=""))
  load(paste("bmres_snow-PVM-shared-",n,".Rda",sep=""))
  load(paste("bmres_snow-MPI-shared-",n,".Rda",sep=""))
  
  
