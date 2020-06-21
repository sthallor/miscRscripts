#######################################################################################
# dclean.R - clean drill predictor data according to parameters
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Nov 28, 2017
#######################################################################################
dclean <- function(dt,prm.dc) {

  epsilon <- 1e-5

  # Check that date time column is present
  if (is.null(dt[[prm.dc$timeColName]])) {
    stop(sprintf('FATAL ERROR - dclean - MUST HAVE DATE TIME COLUMN "%s"',prm.dc$timeColName))
  }

  # initialize numeric value conversion count table
  convertCount <- data.frame(name=c('Other_Continuous','Other_Discrete',prm.dc$valueLimits$name,prm.dc$discrete$name))
  convertCount$startingNA <- 0
  convertCount$minNA <- 0
  convertCount$floor <- 0
  convertCount$ceiling <- 0
  convertCount$maxNA <- 0
  convertCount$totalIn <- 0
  convertCount$interpolated <- 0
  convertCount$extrapolated <- 0
  convertCount$endingNA <- 0
  convertCount$totalOut <- 0

  # bring column names to standard names based on lookup table prm.dc$namefixp
  for (i in 1:nrow(prm.dc$namefixp)) {
    if (!is.null(dt[[prm.dc$namefixp$nonStandardName[i]]])) { # Non-standard name i found
      j <- which(names(dt)==prm.dc$namefixp$nonStandardName[i])
      # Convert any NA number codes to NAs prior to possible linear transformations
      dt[dt[!(is.na(dt[,j])),j]==prm.dc$globalNumberCodeNA,j] <- NA
      if (is.null(dt[[prm.dc$namefixp$standardName[i]]])) {
        if (prm.dc$verbose) cat('\nChanging predictor name=',names(dt)[j],' to ',prm.dc$namefixp$standardName[i])
        names(dt)[j] <- prm.dc$namefixp$standardName[i]
      } else if (prm.dc$namefixp$overwrite[i]==TRUE) {
        if (prm.dc$verbose) cat('\nOverwriting existing predictor ',prm.dc$namefixp$standardName[i],
            ' with ',prm.dc$namefixp$nonStandardName[i])
        dt[[prm.dc$namefixp$standardName[i]]] <- NULL
        names(dt)[j] <- prm.dc$namefixp$standardName[i]
      } else {
        if (prm.dc$verbose) cat('\nStandard predictor ',prm.dc$namefixp$standardName[i],
            ' already exists, NOT overwriting with ', prm.dc$namefixp$nonStandardName[i])
      }
    }
  }
  if (prm.dc$verbose) cat('\n\n')

  # Perform any unit conversions here if option chosen and historian reshape option not chosen
  if (prm.dc$convertUOM==TRUE & !prm.dc$reshapeHistorianData) {
    prm.dc$standardUOM$changeFlag <- FALSE
    for (i in 1:nrow(prm.dc$standardUOM)) {
      if (!is.null(dt[[prm.dc$standardUOM$dColName[i]]]) & 
          !is.null(dt[[prm.dc$standardUOM$uomColName[i]]])) {
        uomConvertCount <- length(which(dt[[prm.dc$standardUOM$uomColName[i]]]==prm.dc$standardUOM$inputUnit[i]))
        if (uomConvertCount>0) {
          # We have at least some values to convert from inputUnit to outputUnit
          # Any NAs in uomColName are set to inputUnit
          dt[[prm.dc$standardUOM$uomColName[i]]][is.na(dt[[prm.dc$standardUOM$uomColName[i]]])] <-
            prm.dc$standardUOM$inputUnit[i]
          uomConvertCount <- length(which(dt[[prm.dc$standardUOM$uomColName[i]]]==prm.dc$standardUOM$inputUnit[i]))

          # makes temporary copy of conversion formula and checks for inconsistencies'
          formula <- prm.dc$standardUOM$formula[i]
          if (substr(formula,1,nchar(prm.dc$standardUOM$outputUnit[i])) != prm.dc$standardUOM$outputUnit[i] |
              is.na(grep(prm.dc$standardUOM$inputUnit[i],formula)[1])) {
            if (prm.dc$verbose) cat('\n\ndclean WARNING unit conversion formula ',prm.dc$standardUOM$formula[i],
              ' inconsistent with inputUnit=',prm.dc$standardUOM$inputUnit[i],
              ' and outputUnit=',prm.dc$standardUOM$outputUnit[i])
          } else {
            # Some units in dColName are being converted, sets flag so unit indicator uomColName is also updated
            prm.dc$standardUOM$changeFlag[i] <- TRUE
            # fixes formula with 'output' as a function of 'input'
            formula <- paste0(gsub(prm.dc$standardUOM$outputUnit[i],'output',substr(formula,1,nchar(prm.dc$standardUOM$outputUnit[i]))),
                              substr(formula,(nchar(prm.dc$standardUOM$outputUnit[i])+1),nchar(formula)))
            formula <- gsub(prm.dc$standardUOM$inputUnit[i],'input',formula)
            # copies input
            input <- dt[[prm.dc$standardUOM$dColName[i]]][dt[[prm.dc$standardUOM$uomColName[i]]]==prm.dc$standardUOM$inputUnit[i]]
            # calculates output from input+formula
            eval(parse(text=formula))
            # replaces input uoms with output uoms
            dt[[prm.dc$standardUOM$dColName[i]]][dt[[prm.dc$standardUOM$uomColName[i]]]==prm.dc$standardUOM$inputUnit[i]] <- output
            
            if (prm.dc$verbose) cat('\nConverted ',uomConvertCount,prm.dc$standardUOM$uomColName[i],' values in ',
                prm.dc$standardUOM$dColName[i],' from ',
                prm.dc$standardUOM$inputUnit[i],' to ',prm.dc$standardUOM$outputUnit[i],
                ' with ',prm.dc$standardUOM$formula[i])
          }
        }
      } else {
        if (is.null(dt[[prm.dc$standardUOM$uomColName[i]]]) & prm.dc$verbose) cat('\ndclean WARNING ... UOM column ',
            prm.dc$standardUOM$uomColName[i],' not found for testing data column ',
            prm.dc$standardUOM$dColName[i],' for possible unit conversion of ',prm.dc$standardUOM$inputUnit[i],
            ' to ',prm.dc$standardUOM$outputUnit[i])
        if (is.null(dt[[prm.dc$standardUOM$dColName[i]]]) & prm.dc$verbose) cat('\ndclean WARNING ... data column ',
            prm.dc$standardUOM$dColName[i],' not found for testing with UOM column ',
            prm.dc$standardUOM$uomColName[i],' for possible unit conversion of ',prm.dc$standardUOM$inputUnit[i],
            ' to ',prm.dc$standardUOM$outputUnit[i])
      }
    }
    # For dColName columns with unit conversions, update the uomColName unit values accordingly
    for (i in 1:nrow(prm.dc$standardUOM)) {
      if (prm.dc$standardUOM$changeFlag[i]==TRUE) {
        dt[[prm.dc$standardUOM$uomColName[i]]] <- as.character(dt[[prm.dc$standardUOM$uomColName[i]]])
        dt[[prm.dc$standardUOM$uomColName[i]]][dt[[prm.dc$standardUOM$uomColName[i]]]==prm.dc$standardUOM$inputUnit[i]] <- 
          prm.dc$standardUOM$outputUnit[i]
      }
    }
  }
  if (prm.dc$verbose) cat('\n\n')

  # Clean up potential stray character between date and time
  # Convert to UTC time if option selected
  if (prm.dc$timeAvg==TRUE) {
    dt[[prm.dc$timeColName]] <- paste0(substr(dt[[prm.dc$timeColName]],1,10),' ',substr(dt[[prm.dc$timeColName]],12,19))
  } else {
    # with no time averaging, truncates last digit to round to nearest 10 seconds
    dt[[prm.dc$timeColName]] <- paste0(substr(dt[[prm.dc$timeColName]],1,10),' ',substr(dt[[prm.dc$timeColName]],12,18),'0')
  }

  if(prm.dc$UTCOffset==TRUE & sum(colnames(dt) %in% "UTC_Offset")>0) {
    if (prm.dc$verbose) cat('\nUsing UTC_Offset to convert EDR_DateTime to UTC times.\n')
    dt$UTC_Offset <- as.character(dt$UTC_Offset)
    dt$time <- as.POSIXlt((as.numeric(dt$time)+
                             as.integer(substr(dt$UTC_Offset,2,3))*3600+
                             as.integer(substr(dt$UTC_Offset,5,6))*60),
                          origin="1970-01-01 00:00:00")
  } else {
    dt$time <- as.POSIXlt(dt[[prm.dc$timeColName]],"%Y-%m-%d %H:%M:%S",tz="")
  }
  
  if (sum(is.na(dt$time))>0) {
    if (prm.dc$verbose) cat('\n\ndclean WARNING ... deleting ',sum(is.na(dt$time)),' observations with invalid time stamp.')
    if (prm.dc$verbose) cat('\nFirst 3 time values = ',head(dt[[prm.dc$timeColName]][is.na(dt$time)],n=3))
    if (prm.dc$verbose) cat('\nThis could be caused by daylight savings time ...\n')
    dt <- dt[!is.na(dt$time),]
  }

  # If save all option not used, reduces columns to only those explicitly selected
  if (!prm.dc$saveAll) {
    dt <- dt[,colnames(dt) %in% c(prm.dc$timeColName,'time',prm.dc$valueLimits$name,prm.dc$discrete$name)]
    if (prm.dc$verbose) cat('\nSave all option not used, only selected columns will be cleaned and returned : \n')
    savedColumns <- data.frame(colname=colnames(dt))
    savedColumns$class <- 'Blank'
    for (i in 1:length(colnames(dt))) { savedColumns$class[i] <- class(dt[[colnames(dt)[i]]])[1] }
    print(savedColumns)
  }

  # Loop through the columns, set NAs, floor & ceiling, track statistics
  for (c in 1:ncol(dt)) {
    if (class(dt[,c])[1]=='numeric' | class(dt[,c])[1]=='integer') {
      if (colnames(dt)[c] %in% prm.dc$valueLimits$name) {
        vli <- which(prm.dc$valueLimits$name %in% colnames(dt)[c]) # valueLimits index
        cci <- which(convertCount$name %in% colnames(dt)[c]) # convertCount index
        # Adjusts total incoming observations count
        convertCount$totalIn[cci] <- convertCount$totalIn[cci] + nrow(dt)
        # Sets values equal to globalNumberCodeNA to NA
        dt[which(dt[,c]==prm.dc$globalNumberCodeNA),c] <- NA
        # Starting NA count
        convertCount$startingNA[cci] <- sum(is.na(dt[,c]))
        # Sets values below minNA to NA
        convertCount$minNA[cci] <- convertCount$minNA[cci] + sum(dt[,c]<prm.dc$valueLimits$minNA[vli],na.rm=T)
        dt[which(dt[,c]<prm.dc$valueLimits$minNA[vli]),c] <- NA
        # Sets values above maxNA to NA
        convertCount$maxNA[cci] <- convertCount$maxNA[cci] + sum(dt[,c]>prm.dc$valueLimits$maxNA[vli],na.rm=T)
        dt[which(dt[,c]>prm.dc$valueLimits$maxNA[vli]),c] <- NA
        # Sets values below floor to floor
        convertCount$floor[cci] <- convertCount$floor[cci] + sum(dt[,c]<prm.dc$valueLimits$floor[vli],na.rm=T)
        dt[which(dt[,c]<prm.dc$valueLimits$floor[vli]),c] <- prm.dc$valueLimits$floor[vli]
        # Sets values above ceiling to ceiling
        convertCount$ceiling[cci] <- convertCount$ceiling[cci] + sum(dt[,c]>prm.dc$valueLimits$ceiling[vli],na.rm=T)
        dt[which(dt[,c]>prm.dc$valueLimits$ceiling[vli]),c] <- prm.dc$valueLimits$ceiling[vli]
        
        # Trap for all NAs and numeric value assigned for NAs, FHS Nov 6, 2016
        if (sum(!is.na(dt[,c]))==0) {
          if(suppressWarnings(!is.na(as.numeric(prm.dc$valueLimits$NANum[vli])))) {
            dt[,c] <- as.numeric(prm.dc$valueLimits$NANum[vli])
          }
        }
      } else { # Numeric but not one of the continuous selected columns, apply global parameters
        if (colnames(dt)[c] %in% prm.dc$discrete$name) {
          cci <- which(convertCount$name %in% colnames(dt)[c]) # convertCount index
        } else { # not a selected column
          # Test for NA limits whether continuous or discrete
          cci <- if (length(unique(dt[,c]))>=prm.dc$contMinUnique & sum(!is.na(dt[,c]))/nrow(dt)>prm.dc$contMinDensity) 1 else 2
        }
        # Adjusts total observations count
        convertCount$totalIn[cci] <- convertCount$totalIn[cci] + nrow(dt)
        # Sets values equal to globalNumberCodeNA to NA
        dt[which(dt[,c]==prm.dc$globalNumberCodeNA),c] <- NA
        # Starting NA count
        convertCount$startingNA[cci] <- convertCount$startingNA[cci] + sum(is.na(dt[,c]))
        # Sets values below globalMinNA to NA
        convertCount$minNA[cci] <- convertCount$minNA[cci] + sum(dt[,c]<prm.dc$globalMinNA,na.rm=T)
        dt[which(dt[,c]<prm.dc$globalMinNA),c] <- NA
        # Sets values above globalMaxNA to NA
        convertCount$maxNA[cci] <- convertCount$maxNA[cci] + sum(dt[,c]>prm.dc$globalMaxNA,na.rm=T)
        dt[which(dt[,c]>prm.dc$globalMaxNA),c] <- NA
      }
    }
  }

  # If option selected, Group to time intervals, average/sample values
  if (prm.dc$timeAvg=='TRUE') {
    library(lubridate)
    library(data.table)
    
    dt$timeSec <- as.factor(as.character(as.POSIXlt(
      # Removed force_tz because of error in R3.4.2 FHS, Nov 27, 2017
      # Seems to work fine in R3.2.2
#       round((as.numeric(force_tz(dt$time,tzone=""))-prm.dc$timeOffset+epsilon)/prm.dc$timeInterval)*prm.dc$timeInterval+prm.dc$timeOffset,
#       origin="1970-01-01 00:00:00")))
      round((as.numeric(dt$time)-prm.dc$timeOffset+epsilon)/prm.dc$timeInterval)*prm.dc$timeInterval+prm.dc$timeOffset,
      origin="1970-01-01 00:00:00")))
    dt1 <- data.frame(time=unique(dt$timeSec))  # unique time records rounded to nearest time interval

    for (c in 1:ncol(dt)) {
      if (sum(!is.na(dt[,c]))>0) { # Must have at least one value to aggregate
        if (prm.dc$verbose) cat('\nAggregating ',colnames(dt)[c],class(dt[,c]))
        if (class(dt[,c])[1]=='numeric' | class(dt[,c])[1]=='integer') {
          if (length(table(dt[,c]))>=prm.dc$contMinUnique) {
            # Takes average for continuous data
            # dataSec <- aggregate(.~timeSec, data=dt[,c(c,ncol(dt))], FUN=function(x){mean(x,na.rm=T)})
            dtable <- data.table(dt[!is.na(dt[,c]),c(c,ncol(dt))])
            colnames(dtable) <- c('v1','timeSec')
            dataSec <- dtable[,list(v1=mean(v1)),by='timeSec']
            if (prm.dc$verbose) cat(' continuous average ')
          } else {
            # Takes the first non NA value for discrete data
            # dataSec <- aggregate(.~timeSec, data=dt[,c(c,ncol(dt))], FUN=function(x){as.numeric(names(table(x))[1])})
            dtable <- data.table(dt[!is.na(dt[,c]),c(c,ncol(dt))])
            colnames(dtable) <- c('v1','timeSec')
            dataSec <- dtable[,list(v1=head(v1,n=1)),by='timeSec']
            if (prm.dc$verbose) cat(' discrete numeric first values ')
          }
          if (prm.dc$verbose) cat(' with ',nrow(dataSec),' values.')
          
          colnames(dataSec) <- c("time",names(dt)[c])
          dataSec <- dataSec[!is.na(dataSec$time),]
          dt1 <- merge(dt1,dataSec,by='time',all.x=T)
        } else { # Non-numeric columns, such as driller remarks
          if (!(colnames(dt)[c] %in% c(prm.dc$timeColName,'time','timeSec'))) {
            # dt[,c] <- as.character(dt[,c])
            # dataSec <- aggregate(.~timeSec, data=dt[,c(c,ncol(dt))], FUN=function(x){names(sort(table(x),decreasing=TRUE)[1])})
            dtable <- data.table(dt[!is.na(dt[,c]),c(c,ncol(dt))])
            colnames(dtable) <- c('v1','timeSec')
            dataSec <- dtable[,list(v1=head(v1,n=1)),by='timeSec'] 
            if (prm.dc$verbose) cat(' non-numeric first values with ',nrow(dataSec),' values.')
            colnames(dataSec) <- c("time",names(dt)[c])
            dataSec <- dataSec[!is.na(dataSec$time),]
            dt1 <- merge(dt1,dataSec,by='time',all.x=T)
          } else {
            if (prm.dc$verbose) cat(' skipping...')
          }
        }
      }
    }
    dt1$time <- as.POSIXlt(dt1$time)
    if (prm.dc$verbose) cat('\n\nAggregating to ',prm.dc$timeInterval,
        ' second time intervals has reduced row count from ',nrow(dt),' to ',nrow(dt1))
 
    # Insert any missing time intervals into dt1
    dt1$timespanAfter <- 0
    dt1$timespanAfter[1:(nrow(dt1)-1)] <- as.numeric(difftime(dt1$time[2:nrow(dt1)],dt1$time[1:(nrow(dt1)-1)],units='secs'))
    newtimes <- NULL
    for (i in which(dt1$timespanAfter > (prm.dc$timeInterval+1) & dt1$timespanAfter < prm.dc$insertMaxTimespan)) {
      # Use force_tz() to avoid problems when converting back to POSIXct
      # Removed force_tz because of error in R3.4.2 FHS, Nov 28, 2017
      # Seems to work fine in R3.2.2
      # timeValue <- as.numeric(force_tz(as.POSIXlt(dt1$time[i]),tzone=""))
      timeValue <- as.numeric(as.POSIXlt(dt1$time[i]))
      for (j in seq(timeValue+prm.dc$timeInterval,timeValue+dt1$timespanAfter[i]-1,by=prm.dc$timeInterval)) {
        newtimes <- append(newtimes, as.POSIXct(j,origin="1970-01-01 00:00:00"))
      }
    }
    newtimes <- data.frame(time=newtimes)
    if (nrow(newtimes)>0) {
      if (prm.dc$verbose) cat('\n\nInserting ',nrow(newtimes),' blank observations with ',
          prm.dc$timeInterval, ' second time intervals to fill in gaps.')
      dt1 <- merge(dt1,newtimes,by='time',all.x=T,all.y=T)
    }
    dt1$timespanAfter <- NULL

  } else {
    dt1 <- dt
  }

  # Loop through the columns, impute NAs (interpolate/extrapolate), track statistics
  for (c in 1:ncol(dt1)) {
    if (class(dt1[,c])[1]=='numeric' | class(dt1[,c])[1]=='integer') {
      # Attempt to impute any missing values
      res <- impute(dt1[,c],colnames(dt1)[c],prm.dc,convertCount)
      dt1[,c] <- res$dt.col
      convertCount <- res$convertCount
    } else {
      dt1[,c] <- as.character(dt1[,c])
    }
  }

  if (prm.dc$verbose) cat('\n\n')
  if (prm.dc$verbose) print(convertCount)

  return(dt1)
}