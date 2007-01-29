###############################################################
## paRc - option pricing
## author: Stefan Theussl <stefan.theussl@wu-wien.ac.at>
## last update: 29.01.2007
###############################################################


### TODO !!
## plot functions (see .Rnw => method for options)
## function updateyield gets current yield (ie. euribor.org)
## 


payoff <- function(path,x,r) {
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  cl <- optionclass(x)
  type <- optiontype(x)
  T <- maturity(x)
  S <- presentvalue(x)
  strike <- strikeprice(x)
  if(cl == "european"){
    ST = S*exp(sum(path))
    if(type == "call") payoff <- exp(-r*T) * max(ST-strike, 0)
    if(type == "put") payoff <- exp(-r*T) * max(0, strike-ST)
    return(payoff)
  }
  ## TODO: american option
  if(cl == "american"){
    stop("American option not implemented yet")
  }   
}


## extractors
maturity <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$maturity
}

strikeprice <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$strikeprice
}

presentvalue <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$present
}

underlying <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  out <- c(x$mu,x$sigma)
  names(out) <- c("mean","sd")
  out
}

optiontype <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$type
}

optionclass <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$kind
}

priceof <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  if(is.null(x$price)){
    warning("you have to run a simulation first")
    return(NULL)
  }
  x$price
}

## monte carlo simulation
monteCarloSimulation <- function(x,r,n,length,
                                 n.simulations=50,antithetic=TRUE){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  price <- vector()
  dt <- maturity(x)/n
  mu <- underlying(x)[1]
  sigma <- underlying(x)[2]
  for( i in 1:n.simulations){
    eps <- matrix(rnorm(n*length),nrow=n)

    if(antithetic)
      eps <- cbind(eps, -eps)
    path <- (mu - sigma^2/2)*dt + sigma*sqrt(dt)*eps
    payoffs <- apply(path,2,payoff,x=x,r=r)
    price[i] <- mean(payoffs)
    }
  x$price <- mean(price)
  x
}

## generics
as.option <- function(x, ...){
  UseMethod(".as.option")
}

## methods
.as.option.default <- function(x){
  writeLines("no default method implemented yet, use a list as input")
}
.as.option.list <- function(x){
  if(!inherits(x,"list")) stop("'x' not of class 'list'")
  available_types<- c("call","put")
  available_classes <- "european"
  if(is.null(x$kind)) x$kind <- available_classes[1]
  x$type <- tolower(x$type)
  if(!any(x$type==available_types)) stop(paste("'x$type' must be either",available[1],"or",available[2]))
  if(is.null(x$kind)) x$kind <- available_classes[1]
  x$kind <- tolower(x$kind)
  if(!any(x$kind==available_classes)) stop(paste("'x$type' must be either",available[1],"or",available[2]))
  if(!is.numeric(x$strikeprice)) stop("'x$strikeprice' must be a numeric")
  if(!is.numeric(x$present)) stop("'x$present' must be a numeric")
  if(is.null(x$maturity)) x$maturity <- 1
  if(!is.numeric(x$maturity)) stop("'x$maturity' must be a numeric")
  if(is.null(x$mu)) x$mu <- 0
  if(!is.numeric(x$mu)) stop("'x$mu' must be a numeric")
  if(is.null(x$sigma)) x$sigma <- 1
  if(!is.numeric(x$sigma)) stop("'x$sigma' must be a numeric")
     
  class(x) <- "option"
  x
}

print.option <- function(x, ...){
  if(!inherits(x,"option")) stop("'x' not of class 'option'")
  writeLines(paste("A",x$type,"option with strike price",x$strikeprice))
  writeLines(paste("expiring in", x$maturity * 360,"days"))
  if(is.numeric(x$price))
    writeLines(paste(x$type,"price: ",x$price))
  invisible(x)
}
     
## some more functions
## the Wiener process as timeseries

wienerPath <- function(S,n,T){
  h<-T/n
  ts(c(S,S+cumsum(rnorm(n)*sqrt(h))),start=0,frequency=n/T)
}


returns <- function(mu,sigma,n,T){
  eps <- rnorm(n)
  dt <- T/n
  path = (mu-sigma^2/2)*dt + sigma*sqrt(dt)*eps
  path
}

