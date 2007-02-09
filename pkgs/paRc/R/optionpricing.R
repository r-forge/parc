###############################################################
## paRc - option pricing
## author: Stefan Theussl <stefan.theussl@wu-wien.ac.at>
## last update: 29.01.2007
###############################################################


### TODO !!
## - plot functions (see .Rnw => method for options)
## - function updateyield gets current yield (ie. euribor.org)
## - coercing option to option
## - monte carlo simulation: n =30 depends on T?
## - binomial tree plot method and creat method see options, futures, ... p211

payoff <- function(path,x,r) {
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  cl <- optionclass(x)
  type <- optiontype(x)
  T <- maturity(x)
  S <- underlying(x)[3]
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

underlying <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  out <- c(x$mu,x$sigma,x$present)
  names(out) <- c("mean","sd","value")
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

position <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  x$position
}

## replacement functions

'maturity<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  if(!all(c(is.numeric(value),length(value)==len)))
    stop(paste("'value' must be a numeric of length", len))
  x$maturity <- value
  x
}

'strikeprice<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  if(!all(c(is.numeric(value),length(value)==len)))
    stop(paste("'value' must be a numeric of length", len))
  x$strikeprice <- value
  x
}

'underlying<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 3
  if(!all(c(is.vector(value),is.numeric(value),length(value)==len)))
    stop(paste("'value' must be a numeric vector of length", len))
  x$mu <- value[1]
  x$sigma <- value[2]
  x$present <- value[3]
  x
}

'optiontype<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  available_types<- c("call","put")
  if(!all(c(is.character(value),length(value)==len)))
    stop(paste("'value' must be a character of length", len))
  value <- tolower(value)
  if(!any(value==available_types)) stop(paste("'value' must be either",available_types[1],"or",available_types[2]))
  x$type <- value
  x
}

'optionclass<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  available_classes <- c("european","american")
  if(!all(c(is.character(value),length(value)==len)))
    stop(paste("'value' must be a character of length", len))
  value <- tolower(value)
  if(!any(value==available_classes)) stop(paste("'value' must be either",available_classes[1],"or",available_classes[2]))
  x$kind <- value
  x
}

'priceof<-' <- function(x, value){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  if(!all(c(is.numeric(value),length(value)==len)))
    stop(paste("'value' must be a numeric of length", len))
  x$price <- value
  x
}

'position<-' <- function(x){
  if(!inherits(x,"option")) stop("'x' must be of class 'option'")
  len <- 1
  available_positions<- c("long","short")
  if(!all(c(is.character(value),length(value)==len)))
    stop(paste("'value' must be a character of length", len))
  value <- tolower(value)
  if(!any(value==available_positions)) stop(paste("'value' must be either",available_positions[1],"or",available_positions[2]))
  x$position <- value
  x
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

#.as.option.option <- function(x){
#  x <- unclass(x)
#  NextMethod(".as.option")
#}

as.list.option <- function(x){
  unclass(x)
}

.as.option.list <- function(x){
  if(!inherits(x,"list")) stop("'x' not of class 'list'")
  available_types<- c("call","put")
  available_positions<- c("long","short")
  available_classes <- c("european","american")
  if(is.null(x$kind)) x$kind <- available_classes[1]
  x$type <- tolower(x$type)
  if(!any(x$type==available_types)) stop(paste("'x$type' must be either",available_types[1],"or",available_types[2]))
  if(is.null(x$kind)) x$kind <- available_classes[1]
  x$kind <- tolower(x$kind)
  if(!any(x$kind==available_classes)) stop(paste("'x$type' must be either",available_classes[1],"or",available_classes[2]))
  if(is.null(x$position)) x$position <- "long"
  x$position <- tolower(x$position)
  if(!any(x$position==available_positions)) stop(paste("'x$position' must be either",available_positions[1],"or",available_positions[2]))
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

## plot method

plot.option <- function(x, ...){
  if(!inherits(x,"option")) stop("'x' not of class 'option'")
  position <- position(x)
  if(position=="long")
    pos <- 1
  else if(position=="short")
    pos <- -1
  else stop("'position' must be 'short' or 'long'")

  type <- optiontype(x)
  cl <- optionclass(x)
  strike <- strikeprice(x)
  price <- priceof(x)
  if(is.null(price)){
    warning("no price calculated yet, using '0'")
    price <- 0
  }
  int <- strike/2
  x <- seq(from=strike-int,to=strike+int,by=strike/100)
  if(type == "call") y <- (apply(matrix(
       c(x-strike,rep(0,length(x))),ncol=2),1,max)-price)*pos
  if(type == "put") y <- (apply(matrix(
       c(strike-x,rep(0,length(x))),ncol=2),1,max)-price)*pos

  main <- paste(cl, type, "option with price",round(price,2))
  plot(x,y,type="l",xlab="stock price",ylab="profit",main=main)
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



