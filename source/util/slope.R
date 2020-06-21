#######################################################################################
# slope.R - calculates slope of y with respect to x with an offset n
# basic calculation is:  slope[i] <- (y[i+n]-y[i-n])/(x[i+n]-x[i-n])
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 11, 2017
#######################################################################################
# x vector of x values
# y vector of y values
# beforeCount before index offset for determining slope
# afterCount after index offset for determining slope
# s smoothing factor, moving average +/- s y values before slope calc 
# eps minimum allowed denominator absolute value

slope <- function(x,y,beforeCount=1,afterCount=1,s=0,eps=1e-6) {
  if (length(x) != length(y)) stop('PROBLEM IN slope..length(x)!=length(y)')
  if (length(x) <= (beforeCount+afterCount)) stop('PROBLEM in slope...length(x)<=beforeCount+afterCount')
  if (beforeCount<0) beforeCount <- 0
  if (afterCount<0) afterCount <- 0
  if (beforeCount==0 & afterCount==0) stop('PROBLEM in slope...must have positive beforeCount or afterCount')
  
  l <- length(x)
  if (s>0) {
    # fv smoothing weights with triangular distribution
    fv <- rep(0,(s*2+1))
    fv[1:(s+1)] <- seq(1,(s+1),1)
    fv[(s+2):length(fv)] <- seq(s,1,-1)
    fv <- fv/sum(fv)
    # cat('\nslope: smoothing factors in y =',fv)
    # smooth y weightrf by fv
    yf <- filter(y,fv,method='convolution',side=2,circular=FALSE)
    yf[1:s] <- y[1:s]
    yf[(l-s+1):l] <- y[(l-s+1):l]
    y <- yf
  }
  
  numerator <- rep(0,l)
  denominator <- rep(0,l)
  # Take care of the ends
  if (beforeCount>0) {
    for (i in 1:beforeCount) {
      numerator[i] <- y[i+afterCount]-y[1]
      denominator[i] <- x[i+afterCount]-x[1]
    }
  }
  if (afterCount>0) {
    for (i in 1:afterCount) {  
      numerator[(l-i+1)] <- y[l]-y[(l-i-beforeCount+1)]
      denominator[(l-i+1)] <- x[l]-x[(l-i-beforeCount+1)]
    }
  }
  # main body of slope calculation
  numerator[(1+beforeCount):(l-afterCount)] <- y[(1+(beforeCount+afterCount)):l]-y[1:(l-(beforeCount+afterCount))]
  denominator[(1+beforeCount):(l-afterCount)] <- x[(1+(beforeCount+afterCount)):l]-x[1:(l-(beforeCount+afterCount))]
  # takes care of near zero and zero values in denominator
  denominator[abs(denominator) < eps] <- eps*sign(denominator[abs(denominator)<eps])
  denominator[denominator==0] <- eps
  return(numerator/denominator)
}


# This is the old slope program, symmetric, that was used until May 11, 2017
slope1 <- function(x,y,n=1,s=0,eps=1e-6) {
  if (length(x) != length(y)) stop('PROBLEM IN slope..length(x)!=length(y)')
  if (length(x) <= (2*n)) stop('PROBLEM in slope...length(x)<=2*n')
  
  l <- length(x)
  if (s>0) {
    # fv smoothing weights with triangular distribution
    fv <- rep(0,(s*2+1))
    fv[1:(s+1)] <- seq(1,(s+1),1)
    fv[(s+2):length(fv)] <- seq(s,1,-1)
    fv <- fv/sum(fv)
    # cat('\nslope: smoothing factors in y =',fv)
    # smooth y weightrf by fv
    yf <- filter(y,fv,method='convolution',side=2,circular=FALSE)
    yf[1:s] <- y[1:s]
    yf[(l-s+1):l] <- y[(l-s+1):l]
    y <- yf
  }
  
  numerator <- rep(0,l)
  denominator <- rep(0,l)
  # Take care of the ends
  for (i in 1:n) {
    numerator[i] <- y[i+n]-y[1]
    denominator[i] <- x[i+n]-x[1]
    numerator[(l-i+1)] <- y[l]-y[(l-i-n+1)]
    denominator[(l-i+1)] <- x[l]-x[(l-i-n+1)]
  }
  # main body of slope calculation
  numerator[(1+n):(l-n)] <- y[(1+2*n):l]-y[1:(l-(2*n))]
  denominator[(1+n):(l-n)] <- x[(1+2*n):l]-x[1:(l-(2*n))]
  # takes care of near zero and zero values in denominator
  denominator[abs(denominator) < eps] <- eps*sign(denominator[abs(denominator)<eps])
  denominator[denominator==0] <- eps
  return(numerator/denominator)
}