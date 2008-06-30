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

define_option <- function(underlying, strike_price, maturity, type = "Call",
                          class = "European", position = "long"){
  x <- list()
  available_types <- c("Call", "Put")
  available_positions <- c("long", "short")
  available_classes <- c("European", "American", "Asian")
  if( !(is.numeric(underlying) || (length(underlying) == 3)) )
    stop("'underlying' must be a numeric vector of length 3")
  x$mu <- underlying[ 1 ]
  x$sigma <- underlying[ 2 ]
  x$present <- underlying[ 3 ]
  x$maturity <- maturity
  if( !is.numeric(strike_price) )
    stop("'strike_price' must be a numeric")
  x$strike_price <- strike_price
  if( !is.numeric(maturity) )
    stop("'maturity' must be a numeric")
  x$maturity <- maturity
  x$type <- type
  if( !any(x$type == available_types) )
    stop(paste("'type' must be either", available_types[ 1 ],
               "or", available_types[ 2 ]))
  x$kind <- class
  if( !any(x$kind == available_classes))
    stop(paste("'class' must be either", available_classes[ 1 ],
               "or", available_classes[ 2 ]))
  x$position <- tolower(position)
  if(!any(x$position == available_positions))
    stop(paste("'position' must be either", available_positions[ 1 ],
               "or", available_positions[ 2 ]))     
  class(x) <- "option"
  x
}

payoff <- function(path, x, r) {
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  cl <- option_class(x)
  type <- option_type(x)
  T <- maturity(x)
  S <- underlying(x)[ 3 ]
  strike <- strike_price(x)
  if( cl == "European" ){
    ST = S * exp(sum(path))
    if( type == "Call" ) pay <- exp(-r * T) * max(ST - strike, 0)
    if( type == "Put"  ) pay <- exp(-r * T) * max(0, strike - ST)
    return(pay)
  }
  if( cl == "Asian" ){
    St = S * cumprod(exp(path))
    if( type == "Call" ) pay <- exp(-r * T) * max(mean(St) - strike, 0)
    if( type == "Put"  ) pay <- exp(-r * T) * max(0, strike - mean(St))
    return(pay)
  }
  ## TODO: american option
  if( cl == "American" ){
    stop("American option not implemented yet")
  }   
}

## extractors
maturity <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  x$maturity
}

strike_price <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  x$strike_price
}

underlying <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  out <- c(x$mu, x$sigma, x$present)
  names(out) <- c("mean", "sd", "value")
  out
}

option_type <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  x$type
}

option_class <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  x$kind
}

price_of <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  if( is.null(x$price) ){
    warning("There is no price available yet")
    return(NULL)
  }
  x$price
}

price_path <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  if( is.null(x$price_path) ){
    warning("You have to run a simulation first")
    return(NULL)
  }
  x$price_path
}

position <- function(x){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  x$position
}

## replacement functions

'maturity<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  if( !all(c(is.numeric(value), length(value) == len)) )
    stop(paste("'value' must be a numeric of length", len))
  x$maturity <- value
  x
}

'strike_price<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  if( !all(c(is.numeric(value), length(value) == len)) )
    stop(paste("'value' must be a numeric of length", len))
  x$strike_price <- value
  x
}

'underlying<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 3
  if( !all(c(is.vector(value), is.numeric(value), length(value) == len)) )
    stop(paste("'value' must be a numeric vector of length", len))
  x$mu <- value[ 1 ]
  x$sigma <- value[ 2 ]
  x$present <- value[ 3 ]
  x
}

'option_type<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  available_types <- c("Call", "Put")
  if( !all(c(is.character(value), length(value) == len)) )
    stop(paste("'value' must be a character of length", len))
  #value <- tolower(value)
  if( !any(value == available_types) )
    stop(paste("'value' must be either", available_types[ 1 ],
               "or", available_types[ 2 ]))
  x$type <- value
  x
}

'option_class<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  available_classes <- c("european","american")
  if( !all(c(is.character(value), length(value) == len)) )
    stop(paste("'value' must be a character of length", len))
  value <- tolower(value)
  if( !any(value == available_classes) )
    stop(paste("'value' must be either", available_classes[ 1 ],
               "or", available_classes[ 2 ]))
  x$kind <- value
  x
}

'price_of<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  if( !all(c(is.numeric(value), length(value) == len)) )
    stop(paste("'value' must be a numeric of length", len))
  x$price <- value
  x
}

'price_path<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  if( !is.numeric(value) )
    stop(paste("'value' must be a vector"))
  x$price_path <- value
  x
}

'position<-' <- function(x, value){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  len <- 1
  available_positions <- c("long", "short")
  if( !all(c(is.character(value), length(value) == len)) )
    stop(paste("'value' must be a character of length", len))
  value <- tolower(value)
  if( !any(value == available_positions) )
    stop(paste("'value' must be either", available_positions[ 1 ],
               "or", available_positions[ 2 ]))
  x$position <- value
  x
}

## black scholes model---analytical solution
Black_Scholes_price <- function(x, r){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  if(option_class(x) != "European")
    stop("Only the price of European options can be calculated")
  sigma <- underlying(x)[ 2 ]
  S0 <- underlying(x)[ 3 ]
  X <- strike_price(x)
  T <- maturity(x)
  d1 <- (log(S0 / X) + (r + sigma^2/2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  type <- option_type(x)
  if(type == "Call")
    out <- S0 * pnorm(d1) - X * exp(-r * T) * pnorm(d2)
  if(type == "Put")
    out <-  X * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)
  out
}

## monte carlo simulation of European options
Monte_Carlo_simulation <- function(x, r, path_length, n_paths,
                                   n_simulations = 50, antithetic = TRUE){
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  price <- numeric(n_simulations)
  dt <- maturity(x) / path_length
  ## in a risk neutral world mu equals to r
  mu <- r
  sigma <- underlying(x)[2]
  for( i in 1:n_simulations){
    eps <- matrix(rnorm(path_length * n_paths), nrow = path_length)

    if(antithetic)
      eps <- cbind(eps, -eps)
    path <- (mu - sigma^2/2) * dt + sigma * sqrt(dt) * eps
    payoffs <- apply(path, 2, payoff, x = x, r = r)
    price[i] <- mean(payoffs)
  }
  price_path(x) <- price
  price_of(x) <- mean(price)
  x
}

mcs.Rmpi.slave <- function(){
  require("paRc")
  commrank <- mpi.comm.rank() - 1
  Monte_Carlo_slave <- function(x, r, path_length, n_paths,
                                n_simulations = 50, antithetic = TRUE){
    price <- vector()
    dt <- maturity(x) / path_length
    ## in a risk neutral world we use r as mu
    mu <- r
    sigma <- underlying(x)[ 2 ]
    for( i in 1:n_simulations ){
      eps <- matrix(rnorm(path_length * n_paths), nrow = path_length)      
      if(antithetic)
        eps <- cbind(eps, -eps)
      path <- (mu - sigma^2/2) * dt + sigma * sqrt(dt) * eps
      payoffs <- apply(path, 2, payoff, x = x, r = r)
      price[ i ] <- mean(payoffs)
    }
    price
  }
  if(commrank == 0)
    local_prices <- Monte_Carlo_slave(x, r, path_length, n_paths, nsim_on_last,
                                      antithetic)
  else
    local_prices <- Monte_Carlo_slave(x, r, path_length, n_paths, nsim,
                                      antithetic)
  mpi.gather.Robj(local_prices, root = 0, comm = 1)    
}

mcs.Rmpi <- function(x, r, path_length, n_paths, n_simulations = 50,
                     antithetic = TRUE, n_cpu = 1, spawnRslaves = FALSE,
                     debug = FALSE) {
  if( !inherits(x, "option") )
    stop("'x' must be of class 'option'")
  
  if( n_cpu == 1 )
    return(Monte_Carlo_simulation(x, r, path_length, n_paths, n_simulations,
                                antithetic))
  if( spawnRslaves == TRUE ){
    mpi.spawn.Rslaves(nslaves = n_cpu)
    mpi.setup.sprng()
  }

  ## broadcast data and functions necessary on slaves
  mpi.bcast.Robj2slave(x) 
  mpi.bcast.Robj2slave(r) 
  mpi.bcast.Robj2slave(path_length)
  mpi.bcast.Robj2slave(n_paths)
  mpi.bcast.Robj2slave(antithetic)
  
  nsim <- ceiling(n_simulations / n_cpu)
  nsim_on_last <- n_simulations - (n_cpu - 1) * nsim
  mpi.bcast.Robj2slave(nsim)
  mpi.bcast.Robj2slave(nsim_on_last)
  mpi.bcast.Robj2slave(mcs.Rmpi.slave)
  ## start partial matrix multiplication on slaves
  mpi.bcast.cmd(mcs.Rmpi.slave())

  ## gather partial results from slaves
  local_prices <- NULL
  prices <- mpi.gather.Robj(local_prices, root = 0, comm = 1)
  #out <- NULL

  if( spawnRslaves == TRUE )
    mpi.close.Rslaves()
  if(debug == TRUE)
    return(prices)
  mean(unlist(prices))
}


## generics
as.option <- function(x, ...){
  UseMethod("as.option")
}

## methods
as.option.default <- function(x){
  writeLines("no default method implemented yet, use a list as input")
}

#as.option.option <- function(x){
#  x <- unclass(x)
#  NextMethod(".as.option")
#}

as.list.option <- function(x){
  unclass(x)
}

as.option.list <- function(x){
  if( !inherits(x, "list") )
    stop("'x' not of class 'list'")
  available_types <- c("call", "put")
  available_positions <- c("long", "short")
  available_classes <- c("european", "american", "asian")
  if( is.null(x$kind) )
    x$kind <- available_classes[ 1 ]
  x$type <- tolower(x$type)
  if( !any(x$type == available_types) )
    stop(paste("'x$type' must be either", available_types[ 1 ],
               "or", available_types[ 2 ]))
  if( is.null(x$kind) )
    x$kind <- available_classes[ 1 ]
  x$kind <- tolower(x$kind)
  if( !any(x$kind == available_classes) )
    stop(paste("'x$type' must be either", available_classes[ 1 ],
               "or", available_classes[ 2 ]))
  if( is.null(x$position) )
    x$position <- "long"
  x$position <- tolower(x$position)
  if( !any(x$position == available_positions) )
    stop(paste("'x$position' must be either", available_positions[ 1 ],
               "or", available_positions[ 2 ]))
  if( !is.numeric(x$strike_price) )
    stop("'x$strike_price' must be a numeric")
  if( !is.numeric(x$present) )
    stop("'x$present' must be a numeric")
  if( is.null(x$maturity) )
    x$maturity <- 1
  if( !is.numeric(x$maturity) )
    stop("'x$maturity' must be a numeric")
  if( is.null(x$mu) )
    x$mu <- 0
  if( !is.numeric(x$mu) )
    stop("'x$mu' must be a numeric")
  if( is.null(x$sigma) )
    x$sigma <- 1
  if( !is.numeric(x$sigma) )
    stop("'x$sigma' must be a numeric")
     
  class(x) <- "option"
  x
}

print.option <- function(x, ...){
  if( !inherits(x,"option") )
    stop("'x' not of class 'option'")
  writeLines(paste("A", x$type, "option with strike price", x$strike_price))
  writeLines(paste("expiring in", x$maturity * 360,"days"))
  if( is.numeric(x$price))
    writeLines(paste(x$type,"price: ", x$price))
  invisible(x)
}

## plot method

plot.option <- function(x, ...){
  if( !inherits(x, "option") )
    stop("'x' not of class 'option'")
  position <- position(x)
  if( position == "long" )
    pos <- 1
  else if(position == "short")
    pos <- -1
  else stop("'position' must be 'short' or 'long'")

  type <- option_type(x)
  cl <- option_class(x)
  strike <- strike_price(x)
  price <- price_of(x)
  if( is.null(price) ){
    warning("no price calculated yet, using '0'")
    price <- 0
  }
  int <- strike / 2
  x <- seq(from = strike - int, to = strike + int, by = strike / 100)
  if(type == "Call")
    y <- (apply(matrix(c(x - strike, rep(0, length(x))), ncol = 2),
                1, max) - price) * pos
  if(type == "Put")
    y <- (apply(matrix(c(strike - x, rep(0, length(x))), ncol = 2),
                1, max) - price) * pos

  main <- paste(cl, type, "Option with Price", round(price, 2))
  plot(x, y, type = "l", xlab = "stock price", ylab = "profit", main = main)
  invisible(x)
}

## some more functions
## the Wiener process as timeseries

Wiener_path <- function(S, n, T){
  h <- T / n
  ts(c(S, S + cumsum(rnorm(n)*sqrt(h))), start = 0, frequency = n / T)
}

returns <- function(mu, sigma, n, T){
  eps <- rnorm(n)
  dt <- T / n
  path = (mu - sigma^2/2) * dt + sigma * sqrt(dt) * eps
  path
}
