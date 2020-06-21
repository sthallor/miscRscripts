#######################################################################################
# runmeanA.R V3.0 - function for asymmetric runmean calculations beforeCount != afterCount
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 22, 2019
#######################################################################################

library(caTools)

# Slow but robust running mean calculation
runmeanSlow <- function(x,beforeCount,afterCount) {
  if (beforeCount<0) beforeCount <- 0 # number of elements to the left
  if (afterCount<0) afterCount <- 0 # number of elements to the right
  # cat('\nrunmeanSlow length(x)=',length(x),' beforeCount=',beforeCount,' afterCount=',afterCount)
  
  y <- rep(0,length(x))
  for (i in 1:length(x)) {
    i1 <- max(1, i-beforeCount)
    i2 <- min(i+afterCount,length(x))
    # cat('\nfor i=',i,' i1=',i1,' i2=',i2)
    y[i] <- mean(x[i1:i2],na.rm=T)
  }
  return(y)
}

# Selected fast running mean calculations
runmeanA <- function(x,beforeCount,afterCount) {
  if (beforeCount<0) beforeCount <- 0 # number of elements to the left
  if (afterCount<0) afterCount <- 0 # number of elements to the right
  totalCount <- beforeCount+1+afterCount # total elements including center
  
  # default value = 0
  y <- rep(0,length(x))
  # cat('\nrunmeanB length(x)=',length(x),' beforeCount=',beforeCount,' afterCount=',afterCount,' totalCount=',totalCount)
  
  if (beforeCount == afterCount) {
    # symetric before & after counts
    if (totalCount<length(x)) {
      # doesn't span entire length of x, so simple pass through to fast runmean
      y <- runmean(x,totalCount,align='center',endrule='mean')
    } else {
      # total count does span length of x, use slow calculation that is correct
      y <- runmeanSlow(x,beforeCount,afterCount)
    }
  } else {
    # we now have asymetric before & after counts
    if (beforeCount==0) {
      # runmean entirely after current record
      y <- runmean(x,totalCount,align='left')
    } else if (afterCount==0) {
      # runmean entirely before current record
      y <- runmean(x,totalCount,align='right')
    } else {
      # we now have asymetric non-zero before & after counts
      if (beforeCount >= length(x) | afterCount >= length(x) | totalCount >= length(x)) {
        # do slow runmean to avoid array index problems
        y <- runmeanSlow(x,beforeCount,afterCount)
      } else {
        # we now have asymetric non-zero before & after counts smaller than length(x)
        # compute from left with offset to the right
        y[(1+beforeCount):length(y)] <- runmean(x,totalCount,align='left')[1:(length(y)-beforeCount)]
        # compute from right with offset to the left
        y[1:(length(y)-afterCount)] <- runmean(x,totalCount,align='right')[(1+afterCount):length(y)]
      }
    }
  }
  return(y)
}

# Slow but robust running sd calculation
runsdSlow <- function(x,beforeCount,afterCount) {
  if (beforeCount<0) beforeCount <- 0 # number of elements to the left
  if (afterCount<0) afterCount <- 0 # number of elements to the right
  # cat('\nrunsdSlow length(x)=',length(x),' beforeCount=',beforeCount,' afterCount=',afterCount)
  
  y <- rep(0,length(x))
  for (i in 1:length(x)) {
    i1 <- max(1, i-beforeCount)
    i2 <- min(i+afterCount,length(x))
    # cat('\ni=',i,' i1=',i1,' i2=',i2)
    if ((i2-i1)>1) y[i] <- sd(x[i1:i2],na.rm=T)
  }
  return(y)
}

# Selected fast running sd calculations
runsdA <- function(x,beforeCount,afterCount) {
  if (beforeCount<0) beforeCount <- 0 # number of elements to the left
  if (afterCount<0) afterCount <- 0 # number of elements to the right
  totalCount <- beforeCount+1+afterCount # total elements including center
  
  # default value = 0
  y <- rep(0,length(x))
  # cat('\nrunsdB length(x)=',length(x),' beforeCount=',beforeCount,' afterCount=',afterCount,' totalCount=',totalCount)
  
  if (beforeCount == afterCount) {
    # symetric before & after counts
    if (totalCount<length(x)) {
      # doesn't span entire length of x, so simple pass through to fast runsd
      if (totalCount > 2) {
        y <- runsd(x,totalCount,align='center',endrule='sd')
      } else {
        y = rep(0,length(x))
      }
    } else {
      # total count does span length of x, use slow calculation that is correct
      y <- runsdSlow(x,beforeCount,afterCount)
    }
  } else {
    # we now have asymetric before & after counts
    if (beforeCount==0) {
      # runsd entirely after current record
      if (totalCount>2) {
        y <- runsd(x,totalCount,align='left')
      } else {
        y = rep(0,length(x))
      }
    } else if (afterCount==0) {
      # runsd entirely before current record
      if (totalCount>2) {
        y <- runsd(x,totalCount,align='right')
      } else {
        y = rep(0,length(x))
      }
    } else {
      # we now have asymetric non-zero before & after counts
      if (beforeCount >= length(x) | afterCount >= length(x) | totalCount >= length(x)) {
        # do slow runsd to avoid array index problems
        y <- runsdSlow(x,beforeCount,afterCount)
      } else {
        # we now have asymetric non-zero before & after counts smaller than length(x)
        # also beforeCount+1+afterCount > 2, so we can safely compute runsd
        # compute from left with offset to the right
        y[(1+beforeCount):length(y)] <- runsd(x,totalCount,align='left')[1:(length(y)-beforeCount)]
        # compute from right with offset to the left
        y[1:(length(y)-afterCount)] <- runsd(x,totalCount,align='right')[(1+afterCount):length(y)]
      }
    }
  }
  y[is.na(y)] <- 0
  return(y)
}


