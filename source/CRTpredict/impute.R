#######################################################################################
# impute.R - missing value (NA) interpolation/extrapolation
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 15, 2017
#######################################################################################
impute <- function(dt.col,col.name,prm.dc,convertCount) {

  NAValue <- (-9999.25) # temporary numeric code for missing values
  continuousFlag <- FALSE
  if (col.name %in% prm.dc$valueLimits$name) {
    cci <- which(convertCount$name %in% col.name) # convertCount index
    maxLook <- prm.dc$valueLimits$maxLook[which(prm.dc$valueLimits$name %in% col.name)]
    continuousFlag <- TRUE
  } else if (col.name %in% prm.dc$discrete$name) {
    cci <- which(convertCount$name %in% col.name) # convertCount index
  } else {
    if (length(unique(dt.col))>=prm.dc$contMinUnique & sum(!is.na(dt.col))/length(dt.col)>prm.dc$contMinDensity)
    {
      cci <- 1 # convertCount index
      maxLook <- prm.dc$contMaxLook
      continuousFlag <- TRUE
    } else {
      cci <- 2 # convertCount index
      maxLook <- prm.dc$discreteMaxLook
      continuousFlag <- FALSE
    }
  }

  # Check if there are any NAs for interpolation/extrapolation
  if (sum(is.na(dt.col))==0) {
    # update counter and return
    convertCount$totalOut[cci] <- convertCount$totalOut[cci] + length(dt.col)
    return(list(dt.col=dt.col,convertCount=convertCount))
  }
  
  # Interpolate/extrapolate - Assign default numeric NAValue to NAs
  # build run lengths and interpolate for NAvalue runs
  dt.col1 <- dt.col
  dt.col1[is.na(dt.col1)] <- NAValue
  
  rlec <- data.frame(values=rle(as.vector(dt.col1))$values,
                     lengths=rle(as.vector(dt.col1))$lengths)
  rlec$from <- 0
  rlec$to <- 0
  rlec$to <- cumsum(rlec$lengths)
  rlec$from <- rlec$to - rlec$lengths + 1
  if (continuousFlag==TRUE) {
    if (nrow(rlec)>=3) { # Minium of 3 run lengths for interpolation
      for (i in 2:(nrow(rlec)-1)) { # must not be on ends to interpolate (only consider from 2 to nrow-1)
        if (rlec$values[i]==NAValue) {
          if ((rlec$to[i]-rlec$from[i]) < (2*maxLook) ) { # interpolate if within span
            i1 <- rlec$from[i]
            i2 <- rlec$to[i]
            convertCount$interpolated[cci] <- convertCount$interpolated[cci]+(i2-i1+1)
            dt.col1[i1:i2] <- seq(rlec$values[i-1],rlec$values[i+1],length.out=(i2-i1+3))[2:(i2-i1+2)]
          } else { # not within span, so extrapolate from above and below
            i2 <- rlec$to[i]
            i1 <- i2-(maxLook-1)
            convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
            dt.col1[i1:i2] <- dt.col1[rlec$from[(i+1)]] # extrapolate from above
            i1 <- rlec$from[i]
            i2 <- i1+(maxLook-1)
            convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
            dt.col1[i1:i2] <- dt.col1[rlec$to[(i-1)]] # extrapolate from below
          }
        }
      }
    }
    if (nrow(rlec)>=2) { # Minimum of 2 run lengths for extrapolation
      # If NAs at begining, then extrapolate from above
      if(rlec$values[1]==NAValue) {
        i2 <- rlec$to[1]
        i1 <- max(1,i2-(maxLook-1))
        convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
        dt.col1[i1:i2] <- dt.col1[rlec$from[2]]
      }
      # If NAs at end, then extrapolate from below
      if(rlec$values[nrow(rlec)]==NAValue) {
        i1 <- rlec$from[nrow(rlec)]
        i2 <- min(length(dt.col1),i1+(maxLook-1))
        convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
        dt.col1[i1:i2] <- dt.col1[rlec$to[(nrow(rlec)-1)]]
      }
    }
  } else { # The discrete case
    if (nrow(rlec)>=2) { # Minimum of 2 run lengths for extrapolation/projection
      if(rlec$values[1]==NAValue) { # First observation is missing, cannot project from prior observation
        i1 <- rlec$from[1]
        i2 <- rlec$to[1]
        convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
        if(length(table(dt.col))==2) { # only two value (i.e. binary 'switch') set to value not encountered first
          if (rlec$values[2]==as.numeric(names(table(dt.col)))[1]) {
            dt.col1[i1:i2] <- as.numeric(names(table(dt.col)))[2]
          } else {
            dt.col1[i1:i2] <- as.numeric(names(table(dt.col)))[1]
          }
        } else { # not a binary switch, extrapolate value from above
          dt.col1[i1:i2] <- rlec$values[2]
        }
      }
      for (i in 2:nrow(rlec)) {
        i1 <- rlec$from[i]
        i2 <- rlec$to[i]
        if (rlec$values[i]==(NAValue)) {
          convertCount$extrapolated[cci] <- convertCount$extrapolated[cci]+(i2-i1+1)
          dt.col1[i1:i2] <- rlec$values[(i-1)]
        }
      }
    }
  }
  
  dt.col1[which(dt.col1==NAValue)] <- NA
  
  # Convert NAs to numeric value if indicated, FHS Oct 25, 2016
  if (col.name %in% prm.dc$valueLimits$name) {
    vli <- which(prm.dc$valueLimits$name %in% col.name) # valueLimits index
    if(suppressWarnings(!is.na(as.numeric(prm.dc$valueLimits$NANum[vli]))) & sum(is.na(dt.col1))>0) {
      if (prm.dc$verbose) cat('\nConverting ',sum(is.na(dt.col1)),' NA values to ',
          prm.dc$valueLimits$NANum[vli],' in column ',col.name)
      dt.col1[which(is.na(dt.col1))] <- as.numeric(prm.dc$valueLimits$NANum[vli])
    }
  }
  
  convertCount$endingNA[cci] <- convertCount$endingNA[cci] + sum(is.na(dt.col1))
  convertCount$totalOut[cci] <- convertCount$totalOut[cci] + length(dt.col1)
  
  return(list(dt.col=dt.col1,convertCount=convertCount))
}