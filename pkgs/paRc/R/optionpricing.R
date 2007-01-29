


## the Wiener process as timeseries

wienerPath <- function(S,n,T){
  h<-T/n
  ts(c(S,S+cumsum(rnorm(n)*sqrt(h))),start=0,frequency=n/T)
}


returns <- function(mu,sigma,n,T){
  eps <- rnorm(n)
  dt <- T/n
  path = (mu-sigma^2/2)*dt + sigma*sqrt(dt)*eps
  # Return Value:
  path
}

europeanPayoff = function(path,S,T,r,type="call") { 
          
          # Compute the Call/Put Payoff Value:
          ST = S*exp(sum(path))
          if (type == "call") payoff <- exp(-r*T) * max(ST-X, 0)
          if (type == "put") payoff <- exp(-r*T) * max(0, X-ST)
          # Return Value:
          payoff
}


monteCarloSimulation <- function(mu,sigma,T,S,r,n,length,
                                 n.simulations=50,antithetic=TRUE){
  price <- vector()
  dt <- T/n
  for( i in 1:n.simulations){
    eps <- matrix(rnorm(n*length),nrow=n)
    #browser()
    if(antithetic)
      eps <- cbind(eps, -eps)
    #browser()
    path <- (mu - sigma^2/2)*dt + sigma*sqrt(dt)*eps

    
    payoffs <- apply(path,2,europeanPayoff,S=S,T=T,r=r)
    price[i] = mean(payoffs)
    }
  price
}
