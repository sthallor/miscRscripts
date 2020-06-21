##########################################################################
# dbuild.R - build feature columns for selecting rig state classifications
# Build data model for both training and classification
# Ensign Energy Services Inc. retains all rights to this software
# FHS Mar 25, 2019
##########################################################################
dbuild <- function(dt,prm.t,eigenRes) {

  library(caTools)

  # Identify any fatal errors before starting computations
  errorFlag <- FALSE
  if (sum(!(prm.t$cp$name %in% colnames(dt))) != 0) {
    cat('\n\nFATAL ERROR in dbuild ... the following requested columns for building calculated predictors are missing...\n')
    cat(prm.t$cp$name[which(!(prm.t$cp$name %in% colnames(dt)))])
    errorFlag <- TRUE
  }
  if (sum(colnames(dt) %in% prm.t$timeColName) != 1) {
    cat('\n\nFATAL ERROR in dbuild... time column ', prm.t$timeColName, ' missing...\n\n')
    errorFlag <- TRUE
  }
  if (is.null(dt$EDR_BitDepth)) {
    if (nchar(prm.t$bitHoleDepthRatioName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_BitDepth that is required for requested calculated predictor',
          prm.t$bitHoleDepthRatioName)
      errorFlag <- TRUE
    }
    if (nchar(prm.t$bitSpeedName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_BitDepth that is required for requested calculated predictor',
          prm.t$bitSpeedName)
      errorFlag <- TRUE
    }
    if (nchar(prm.t$bitDistFromHoleBottomName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_BitDepth that is required for requested calculated predictor',
          prm.t$bitDistFromHoleBottomName)
      errorFlag <- TRUE
    }
  }
  if (is.null(dt$EDR_HoleDepth)) {
    if (nchar(prm.t$bitHoleDepthRatioName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_HoleDepth that is required for requested calculated predictor',
          prm.t$bitHoleDepthRatioName)
      errorFlag <- TRUE
    }
    if (nchar(prm.t$bitDistFromHoleBottomName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_HoleDepth that is required for requested calculated predictor',
          prm.t$bitDistFromHoleBottomName)
      errorFlag <- TRUE
    }
  }
  if (is.null(dt$EDR_BlockHeight)) {
    if (nchar(prm.t$deltaBlockHeightAbsName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_BlockHeight that is required for requested calculated predictor',
          prm.t$deltaBlockHeightAbsName)
      errorFlag <- TRUE
    }
    if (nchar(prm.t$blockHeightRunLengthsName)>0) {
      cat('\n\nFATAL ERROR in dbuild... missing input column EDR_BlockHeight that is required for requested calculated predictor',
          prm.t$blockHeightRunLengthsName)
      errorFlag <- TRUE
    }
  }
  
  if (errorFlag) stop('FATAL ERROR(S) in dbuild ... one or more missing input predictor variables ... ')
  
  # If time column not called 'time', rename it to time
  if (prm.t$timeColName != 'time') {
    colnames(dt)[colnames(dt) %in% prm.t$timeColName] <- 'time'
    if (prm.t$verbose) cat('\nRenaming time column ',prm.t$timeColName,' to time for random forest training/classification.')
  }
  dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
  if(sum(is.na(dt$time))>0) {
    cat('\n\ndbuild WARNING... found ',sum(is.na(dt$time)),' NA dt$time values. Possible corrupt input file.')
  }
  
  if (!is.null(dt$Rig)) dt$Rig <- as.factor(dt$Rig)

  # timespan column for intervals
  dt$timespan <- 0
  dt$timespan[1] <- as.numeric(difftime(dt$time[2],dt$time[1],units='secs'))
  dt$timespan[2:(nrow(dt)-1)] <- as.numeric(difftime(dt$time[3:nrow(dt)],dt$time[1:(nrow(dt)-2)], units='secs'))/2
  dt$timespan[nrow(dt)] <- as.numeric(difftime(dt$time[nrow(dt)],
                                               dt$time[(nrow(dt)-1)],units='secs'))
  # Perform any unit conversions here 
  prm.t$standardUOM$changeFlag <- FALSE
  for (i in 1:nrow(prm.t$standardUOM)) {
    if (!is.null(dt[[prm.t$standardUOM$dColName[i]]]) & 
        !is.null(dt[[prm.t$standardUOM$uomColName[i]]])) {
      uomConvertCount <- length(which(dt[[prm.t$standardUOM$uomColName[i]]]==prm.t$standardUOM$inputUnit[i]))
      if (uomConvertCount>0) {
        # We have at least some values to convert from inputUnit to outputUnit
        # Any NAs in uomColName are set to inputUnit
        dt[[prm.t$standardUOM$uomColName[i]]][is.na(dt[[prm.t$standardUOM$uomColName[i]]])] <-
          prm.t$standardUOM$inputUnit[i]
        uomConvertCount <- length(which(dt[[prm.t$standardUOM$uomColName[i]]]==prm.t$standardUOM$inputUnit[i]))
        
        # makes temporary copy of conversion formula and checks for inconsistencies'
        formula <- prm.t$standardUOM$formula[i]
        if (substr(formula,1,nchar(prm.t$standardUOM$outputUnit[i])) != prm.t$standardUOM$outputUnit[i] |
            is.na(grep(prm.t$standardUOM$inputUnit[i],formula)[1])) {
          if (prm.t$verbose) cat('\n\ndbuild WARNING unit conversion formula ',prm.t$standardUOM$formula[i],
              ' inconsistent with inputUnit=',prm.t$standardUOM$inputUnit[i],
              ' and outputUnit=',prm.t$standardUOM$outputUnit[i])
        } else {
          # Some units in dColName are being converted, sets flag so unit indicator uomColName is also updated
          prm.t$standardUOM$changeFlag[i] <- TRUE
          # fixes formula with 'output' as a function of 'input'
          formula <- paste0(gsub(prm.t$standardUOM$outputUnit[i],'output',substr(formula,1,nchar(prm.t$standardUOM$outputUnit[i]))),
                            substr(formula,(nchar(prm.t$standardUOM$outputUnit[i])+1),nchar(formula)))
          formula <- gsub(prm.t$standardUOM$inputUnit[i],'input',formula)
          # copies input, if factor converts class to numeric
          if (class(dt[[prm.t$standardUOM$dColName[i]]])=='factor') dt[[prm.t$standardUOM$dColName[i]]] <-
            as.numeric(as.character(dt[[prm.t$standardUOM$dColName[1]]]))
          input <- dt[[prm.t$standardUOM$dColName[i]]][dt[[prm.t$standardUOM$uomColName[i]]]==prm.t$standardUOM$inputUnit[i]]
          
          # calculates output from input+formula
          eval(parse(text=formula))

          # turns any infinite values into NAs
          output[is.infinite(output)] <- NA
          # replaces input uoms with output uoms
          dt[[prm.t$standardUOM$dColName[i]]][dt[[prm.t$standardUOM$uomColName[i]]]==prm.t$standardUOM$inputUnit[i]] <- output
          
          if (prm.t$verbose) cat('\nConverted ',uomConvertCount,prm.t$standardUOM$uomColName[i],' values in ',prm.t$standardUOM$dColName[i],' from ',
              prm.t$standardUOM$inputUnit[i],' to ',prm.t$standardUOM$outputUnit[i],
              ' with ',prm.t$standardUOM$formula[i])
        }
      }
    } else {
      if (is.null(dt[[prm.t$standardUOM$uomColName[i]]]) & prm.t$verbose ) cat('\ndbuild WARNING ... UOM column ',
          prm.t$standardUOM$uomColName[i],' not found for testing data column ',
          prm.t$standardUOM$dColName[i],' for possible unit conversion of ',prm.t$standardUOM$inputUnit[i],
          ' to ',prm.t$standardUOM$outputUnit[i])
      if (is.null(dt[[prm.t$standardUOM$dColName[i]]]) & prm.t$verbose) cat('\ndbuild WARNING ... data column ',
          prm.t$standardUOM$dColName[i],' not found for testing with UOM column ',
          prm.t$standardUOM$uomColName[i],' for possible unit conversion of ',prm.t$standardUOM$inputUnit[i],
          ' to ',prm.t$standardUOM$outputUnit[i])
    }
  }
  # For dColName columns with unit conversions, update the uomColName unit values accordingly
  for (i in 1:nrow(prm.t$standardUOM)) {
    if (prm.t$standardUOM$changeFlag[i]==TRUE) {
      dt[[prm.t$standardUOM$uomColName[i]]] <- as.character(dt[[prm.t$standardUOM$uomColName[i]]])
      dt[[prm.t$standardUOM$uomColName[i]]][dt[[prm.t$standardUOM$uomColName[i]]]==prm.t$standardUOM$inputUnit[i]] <- 
        prm.t$standardUOM$outputUnit[i]
    }
  }
  # Section to catch missing EDR_ROP values that can be inferred from hole/bit depth values August 24, 2017
  # If bit is off of hole bottom, then ROP must be zero
  if (!is.null(dt$EDR_HoleDepth) & !is.null(dt$EDR_BitDepth) & !is.null(dt$EDR_ROP)) {
    selected <- which(is.na(dt$EDR_ROP) & (dt$EDR_HoleDepth - dt$EDR_BitDepth > prm.t$maxBitDistFromHoleBottom))
    if (length(selected)>0) {
      cat('\ndbuild - setting ',length(selected),' missing EDR_ROP values to zero where HoleDepth - BitDepth > ',
          prm.t$maxBitDistFromHoleBottom)
      dt$EDR_ROP[selected] <- 0
    }
  }
  
  ######################################################################################################
  # Section for building calculated predictors specified in parms file, March 18, 2019
  
  # Some hardwired data patching 
  
  # Makes sure that all EDR_HoleDepth values are at least 1 (important for bitHoleDepthRatio)
  if (!is.null(dt$EDR_HoleDepth)) {
    dt$EDR_HoleDepth[dt$EDR_HoleDepth<1] <- 1
    dt$EDR_HoleDepth[is.na(dt$EDR_HoleDepth)] <- 1
  }
  
  # Section to catch missing EDR_ROP values that can be inferred from hole/bit depth values August 24, 2017
  # If bit is off of hole bottom, then ROP must be zero
  if (!is.null(dt$EDR_HoleDepth) & !is.null(dt$EDR_BitDepth) & !is.null(dt$EDR_ROP)) {
    selected <- which(is.na(dt$EDR_ROP) & (dt$EDR_HoleDepth - dt$EDR_BitDepth > prm.t$maxBitDistFromHoleBottom))
    if (length(selected)>0) {
      cat('\ndbuild - setting ',length(selected),' missing EDR_ROP values to zero where HoleDepth - BitDepth > ',
          prm.t$maxBitDistFromHoleBottom)
      dt$EDR_ROP[selected] <- 0
    }
  }
  
  if (prm.t$verbose) cat('\n\nBuilding calculated predictors')

  # bitHoleDepthRatio
  if (nchar(prm.t$bitHoleDepthRatioName)>0) {
    if (!is.null(dt$EDR_HoleDepth) & !is.null(dt$EDR_BitDepth)) {
      dt[[prm.t$bitHoleDepthRatioName]] <- dt$EDR_BitDepth/dt$EDR_HoleDepth
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$bitHoleDepthRatioName)
    }
  }
  
  # bitSpeed
  if (nchar(prm.t$bitSpeedName)>0) {
    if (!is.null(dt$EDR_BitDepth)) {
      dt[[prm.t$bitSpeedName]] <- slope(as.numeric(dt$time),dt$EDR_BitDepth,beforeCount=prm.t$beforeCount,afterCount=prm.t$afterCount,s=3)
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$bitSpeedName)
    }
  }
  
  # bitDistFromHoleBottom
  if (nchar(prm.t$bitDistFromHoleBottomName)>0) {
    if (!is.null(dt$EDR_BitDepth) & !is.null(dt$EDR_HoleDepth)) {
      dt[[prm.t$bitDistFromHoleBottomName]] <- dt$EDR_HoleDepth - dt$EDR_BitDepth
      dt[[prm.t$bitDistFromHoleBottomName]][dt[[prm.t$bitDistFromHoleBottomName]]<0] <- 0
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$bitDistFromHoleBottomName)
    }
  }
  
  # deltaBitDepthSmooth
  if (nchar(prm.t$deltaBitDepthSmooth)>0) {
    if (!is.null(dt$EDR_BitDepth)) {
      delta <- rep(0,nrow(dt))
      delta[2:nrow(dt)] <- dt$EDR_BitDepth[2:nrow(dt)]-dt$EDR_BitDepth[1:(nrow(dt)-1)]
      barriers <- which(abs(delta/dt$timespan)>prm.t$maxDeltaBitdepthRate)
      cpname <- prm.t$deltaBitDepthSmooth
      if (length(barriers)>0) {
        dt[[cpname]] <- 0 # Initialize to zero
        for (i in 1:(length(barriers)+1)) { # Loop through each barrier value
          i1 <- if (i==1) 1 else barriers[i-1]+1  # Start with value after previous barrier
          i2 <- if (i>length(barriers)) nrow(dt) else barriers[i]-1 # End with value before next barrier 
          if (i2 - i1 > 1) { # must have some values to smooth
            dt[[cpname]][i1:i2] <- runmeanA(dt$EDR_BitDepth[i1:i2],prm.t$beforeCount,prm.t$afterCount)
          } 
        }
        dt[[cpname]][barriers] <- dt[[cpname]][(barriers-1)] # delta value at barrier set to preceding value
      } else {
        dt[[cpname]] <- runmeanA(dt$EDR_BitDepth,prm.t$beforeCount,prm.t$afterCount)
      }
      # Calculate the deltas from the runmean 'smoothed' bit depth values
      dt[[cpname]][2:nrow(dt)] <- dt[[cpname]][2:nrow(dt)] - dt[[cpname]][1:(nrow(dt)-1)]
      dt[[cpname]][1] <- 0  # delta from first value set to zero
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$deltaBitDepthSmooth)
    }
  }
  
  # deltaHoleDepthSmooth
  if (nchar(prm.t$deltaHoleDepthSmooth)>0) {
    if (!is.null(dt$EDR_HoleDepth)) {
      delta <- rep(0,nrow(dt))
      delta[2:nrow(dt)] <- dt$EDR_HoleDepth[2:nrow(dt)]-dt$EDR_HoleDepth[1:(nrow(dt)-1)]
      barriers <- which(abs(delta/dt$timespan)>prm.t$cp$maxdeltaHoledepthRate)
      cpname <- prm.t$deltaHoleDepthSmooth
      if (length(barriers)>0) {
        dt[[cpname]] <- 0 # Initialize to zero
        for (i in 1:(length(barriers)+1)) { # Loop through each barrier value
          i1 <- if (i==1) 1 else barriers[i-1]+1  # Start with value after previous barrier
          i2 <- if (i>length(barriers)) nrow(dt) else barriers[i]-1 # End with value before next barrier 
          if (i2 - i1 > 1) { # must have some values to smooth
            dt[[cpname]][i1:i2] <- runmeanA(dt$EDR_HoleDepth[i1:i2],prm.t$beforeCount,prm.t$afterCount)
          } 
        }
        dt[[cpname]][barriers] <- dt[[cpname]][(barriers-1)] # delta value at barrier set to preceding value
      } else {
        dt[[cpname]] <- runmeanA(dt$EDR_HoleDepth,prm.t$beforeCount,prm.t$afterCount)
      }
      # Calculate the deltas from the runmean 'smoothed' hole depth values
      dt[[cpname]][2:nrow(dt)] <- dt[[cpname]][2:nrow(dt)] - dt[[cpname]][1:(nrow(dt)-1)]
      dt[[cpname]][1] <- 0  # delta from first value set to zero
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$deltaHoleDepthSmooth)
    }
  }
  
  # deltaBlockHeightAbs
  if (nchar(prm.t$deltaBlockHeightAbsName)>0) {
    if (!is.null(dt$EDR_BlockHeight)) {
      dt[[prm.t$deltaBlockHeightAbsName]] <- 0
      dt[[prm.t$deltaBlockHeightAbsName]][2:nrow(dt)] <- abs(dt$EDR_BlockHeight[2:nrow(dt)]-dt$EDR_BlockHeight[1:(nrow(dt)-1)])
      dt[[prm.t$deltaBlockHeightAbsName]][is.na(dt[[prm.t$deltaBlockHeightAbsName]])] <- 0
      temp <- cumsum(dt[[prm.t$deltaBlockHeightAbsName]])
      bc <- prm.t$beforeCount
      ac <- prm.t$afterCount
      tc <- bc+ac+1
      if (tc < length(temp)) {
        # The main block calculation
        # set as difference between cumsums with bc & ac offsets
        dt[[prm.t$deltaBlockHeightAbsName]][(bc+1):(nrow(dt)-(ac+1))] <- temp[(tc+1):nrow(dt)] - temp[1:(nrow(dt)-tc)]
        # The front end calculation, set equal to last calculated value
        if (bc>0) dt[[prm.t$deltaBlockHeightAbsName]][1:bc] <- dt[[prm.t$deltaBlockHeightAbsName]][(bc+1)]					
        # The back end calculation, set equal to last calculated value
        dt[[prm.t$deltaBlockHeightAbsName]][(nrow(dt)-ac):nrow(dt)] <- dt[[prm.t$deltaBlockHeightAbsName]][(nrow(dt)-(ac+1))]
      } else {
        # If tc >= length(temp), set all value equal to total sum over interval 
        dt[[prm.t$deltaBlockHeightAbsName]] <- temp[length(temp)]
      }
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$deltaBlockHeightAbsName)
    }
  }

  # blockHeightRunLengths
  if (nchar(prm.t$blockHeightRunLengthsName)>0) {
    if (!is.null(dt$EDR_BlockHeight)) {
      ra <- data.frame(values=rle(as.vector(dt$EDR_BlockHeight))$values,
                       lengths=rle(as.vector(dt$EDR_BlockHeight))$lengths)
      ra$values <- as.numeric(ra$values)
      # Place constant blockheight value run lengths from rle into predictor dt[[prm.t$blockHeightRunLengthsName]] 
      ra$values <- ra$lengths
      dt[[prm.t$blockHeightRunLengthsName]] <- inverse.rle(ra)
      # cap run length to before & after timespan interval count, FHS Feb 18, 2019
      maxspan <- prm.t$beforeCount + 1 + prm.t$afterCount
      dt[[prm.t$blockHeightRunLengthsName]][dt[[prm.t$blockHeightRunLengthsName]]>maxspan] <- maxspan
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$blockHeightRunLengthsName)
    }
  }
  
  # toolFaceAbsDeltaRunMean
  if (nchar(prm.t$toolFaceAbsDeltaRunMean)>0) {
    if (!is.null(dt$EDR_ToolFace)) {
      temp <- dt$EDR_ToolFace
      temp[temp<0] <- NA
      temp[temp>360] <- NA
      # Compute delta tool face angle (rotation)
      dt[[prm.t$toolFaceAbsDeltaRunMean]] <- 0
      dt[[prm.t$toolFaceAbsDeltaRunMean]][2:nrow(dt)] <- temp[2:nrow(dt)] - temp[1:(nrow(dt)-1)]
      # if delta tool face angle greater than 180 degrees, flips direction
      select <- which(dt[[prm.t$toolFaceAbsDeltaRunMean]] < (-180))
      if (length(select>0)) dt[[prm.t$toolFaceAbsDeltaRunMean]][select] <- dt[[prm.t$toolFaceAbsDeltaRunMean]][select] + 360
      select <- which(dt[[prm.t$toolFaceAbsDeltaRunMean]] > 180)
      if (length(select>0)) dt[[prm.t$toolFaceAbsDeltaRunMean]][select] <- dt[[prm.t$toolFaceAbsDeltaRunMean]][select] - 360
      # Takes absolute value of tool face delta angle
      dt[[prm.t$toolFaceAbsDeltaRunMean]] <- runmeanA(abs(dt[[prm.t$toolFaceAbsDeltaRunMean]]),prm.t$beforeCount,prm.t$afterCount)
      # dt[[prm.t$toolFaceAbsDeltaRunMean]] <- runmean(abs(dt[[prm.t$toolFaceAbsDeltaRunMean]]),(2*prm.t$beforeCount+1))
      # Convert any NA's to -999 
      dt[[prm.t$toolFaceAbsDeltaRunMean]][is.na(dt[[prm.t$toolFaceAbsDeltaRunMean]])] <- -999 
      if (prm.t$verbose) cat('\nBuilt calculated predictor ',prm.t$toolFaceAbsDeltaRunMean)
    }
  }
  

  # Loop through list of standard calculated predictors
  for (p in 1:nrow(prm.t$cp)) {
    if (prm.t$verbose) {
      if (prm.t$cp$name[p] %in% prm.t$standardUOM$dColName) {
        uomrow <- which(prm.t$standardUOM$dColName %in% prm.t$cp$name[p])[1]
        uom <- as.character(dt[[prm.t$standardUOM$uomColName[uomrow]]])[1]
      } else {
        uom <- 'UNK'
      }
      cat('\n\nBuilding calculated predictors based on ',prm.t$cp$name[p],' with UOM=',uom,'\n')
    }
    # Looks for running mean and standard deviation barriers that exceed max delta rate per second
    delta <- rep(0,nrow(dt))
    delta[2:nrow(dt)] <- dt[[prm.t$cp$name[p]]][2:nrow(dt)]-dt[[prm.t$cp$name[p]]][1:(nrow(dt)-1)]
    barriers <- which(abs(delta/dt$timespan)>prm.t$cp$maxdeltarate[p])
    # Create running mean if selected
    if (prm.t$cp$runmean[p]) {
      cpname <- paste(prm.t$cp$name[p],'_runmean',sep='')
      if (length(barriers)>0) {
        dt[[cpname]] <- 0 # Initialize to zero
        for (i in 1:(length(barriers)+1)) { # Loop through each barrier value
          i1 <- if (i==1) 1 else barriers[i-1]+1  # Start with value after previous barrier
          i2 <- if (i>length(barriers)) nrow(dt) else barriers[i]-1 # End with value before next barrier 
          if (i2 - i1 > 1) { # must have some values to smooth
            dt[[cpname]][i1:i2] <- runmeanA(dt[[prm.t$cp$name[p]]][i1:i2],prm.t$beforeCount,prm.t$afterCount)
          } 
        }
        dt[[cpname]][barriers] <- dt[[cpname]][(barriers-1)] # delta value at barrier set to preceding value
      } else {
        dt[[cpname]] <- runmeanA(dt[[prm.t$cp$name[p]]],prm.t$beforeCount,prm.t$afterCount)
      }
      if (prm.t$verbose) {
        cat(cpname,' ')
        # if (length(barriers)>0) cat(' with ',length(barriers),' barriers.')
        # if (sum(is.na(dt[[cpname]]))>0) cat(' Warning... has ',sum(is.na(dt[[cpname]])),' NAs.')
      }
    }
    # create running standard deviation if selected
    if (prm.t$cp$runsd[p]) {
      cpname <- paste(prm.t$cp$name[p],'_runsd',sep='')
      if (length(barriers)>0) {
        dt[[cpname]] <- 0 # Initialize to zero
        for (i in 1:(length(barriers)+1)) { # Loop through each barrier value
          i1 <- if (i==1) 1 else barriers[i-1]+1  # Start with value after previous barrier
          i2 <- if (i>length(barriers)) nrow(dt) else barriers[i]-1 # End with value before next barrier 
          if (i2 - i1 > 1) { # must have some values to smooth
            dt[[cpname]][i1:i2] <- runsdA(dt[[prm.t$cp$name[p]]][i1:i2],prm.t$beforeCount,prm.t$afterCount)
          }
        }
        dt[[cpname]][barriers] <- dt[[cpname]][(barriers-1)]
      } else {
        dt[[cpname]] <- runsdA(dt[[prm.t$cp$name[p]]],prm.t$beforeCount,prm.t$afterCount)
      }
      if (prm.t$verbose) {
        cat(cpname,' ')
        # cat('\nBuilt running sd predictor ',cpname)
        # if (length(barriers)>0) cat(' with ',length(barriers),' barriers.')
        # if (sum(is.na(dt[[cpname]]))>0) cat(' Warning... has ',sum(is.na(dt[[cpname]])),' NAs.')
      }
    }
    # create delta if selected
    if (prm.t$cp$delta[p]) {
      cpname <- paste(prm.t$cp$name[p],'_delta',sep='')
      dt[[cpname]] <- 0
      dt[[cpname]][2:nrow(dt)] <- dt[[prm.t$cp$name[p]]][2:nrow(dt)]-dt[[prm.t$cp$name[p]]][1:(nrow(dt)-1)]
      if (prm.t$verbose) {
        cat(cpname,' ')
        # cat('\nBuilt delta predictor ',cpname)
        # if (sum(is.na(dt[[cpname]]))>0) cat(' Warning... has ',sum(is.na(dt[[cpname]])),' NAs.')
      }
    }
    # create running percentiles if selected
    if (prm.t$cp$percent[p]) {
      cpname <- paste(prm.t$cp$name[p],'_percent',sep='')
      # create vector data padded with beforecount and aftercount
      vd <- c(rep(dt[[prm.t$cp$name[p]]][1],prm.t$beforeCount),
              dt[[prm.t$cp$name[p]]],
              rep(dt[[prm.t$cp$name[p]]][nrow(dt)],prm.t$afterCount))
      tc <- prm.t$beforeCount + 1 + prm.t$afterCount
      rmin <- runmin(vd,tc,align='right',endrule='NA')[tc:length(vd)]
      rmax <- runmax(vd,tc,align='right',endrule='NA')[tc:length(vd)]
      # Make sure that rmax - rmin is always greater than zero
      rmax[(rmax-rmin)<prm.t$deltaHoleBitZeroTolerance] <- prm.t$deltaHoleBitZeroTolerance
      dt[[cpname]] <- 0 # initialize to zero
      dt[[cpname]] <- 100*(dt[[prm.t$cp$name[p]]]-rmin)/(rmax-rmin)
      if (prm.t$verbose) cat(cpname,' ')
    }
    # create eigenvector transformed predictors if selected
    if (prm.t$cp$eigencount[p]>0) {
      if(!prm.t$cp$name[p] %in% names(eigenRes)) {
        cat('\nFATAL ERROR in dbuild eigen PCA model does not contain eigenvectors for ',prm.t$cp$name[p])
      } else {
        # create vector data padded with beforecount and aftercount
        vd <- c(rep(dt[[prm.t$cp$name[p]]][1],prm.t$beforeCount),
                dt[[prm.t$cp$name[p]]],
                rep(dt[[prm.t$cp$name[p]]][nrow(dt)],prm.t$afterCount))
        # cat('\ncreated eigenvector data with ',length(vd),' elements.  nrow(dt)=',nrow(dt))
        # build the M matrix from the padded data vector
        for (i in -prm.t$beforeCount:prm.t$afterCount) {
          if (i==-prm.t$beforeCount) {
            M <- matrix(vd[(i+prm.t$beforeCount+1):(length(vd)+i-prm.t$afterCount)],ncol=1,nrow=nrow(dt))
          } else {
            M <- cbind(M,vd[(i+prm.t$beforeCount+1):(length(vd)+i-prm.t$afterCount)])
          }
        }
        # cat('\nMatrix M has ',nrow(M),' rows and ',ncol(M),' columns.')
        Mpca <- M %*% eigenRes[[prm.t$cp$name[p]]]$vectors[,1:prm.t$cp$eigencount[p]]
        # if (prm.t$verbose) cat('\nEigenvector matrix Mpca has ',nrow(Mpca),' rows and ',ncol(Mpca),' columns.')
        if (prm.t$verbose) cat('and ',ncol(Mpca),' PCA eigenvectors')
        for (p1 in 1:prm.t$cp$eigencount[p]) {
          cpname <- paste(prm.t$cp$name[p],'_ev',as.character(p1),sep='')
          if (!cpname %in% prm.t$tpnames) cat('\nPROBLEM... missing eigenvector=',cpname)
          dt[[cpname]] <- Mpca[,p1]
        }
        
      }
    }
    if (prm.t$verbose & length(barriers)>0) {
      cat('\nWarning... found ',length(barriers),' records with delta rate exceeding',prm.t$cp$maxdeltarate[p],
          uom,'per second maximum\n')
    }
  }

  # Check that all predictor training variables are present
  if (sum(!(prm.t$tpnames %in% colnames(dt)))>0) {
    cat('\n\nFATAL ERROR ... at end of dbuild still missing the following requested predictor variable(s) ...\n')
    cat(prm.t$tpnames[which(!(prm.t$tpnames %in% colnames(dt)))])
    stop('FATAL ERROR in dbuild ... missing predictor variables ... ')
  }
  
  if (prm.t$verbose) cat('\n\nSuccessful dbuild with ',length(prm.t$tpnames),' predictors and ',
                         nrow(dt),' records.\n')
  
  return(dt)
}