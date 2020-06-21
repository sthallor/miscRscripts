##########################################################################
# dbuildPredict.R - build predictor columns for predicting target value
# Build data model for both training and predictions
# Ensign Energy Services Inc. retains all rights to this software
# FHS April 4, 2017
##########################################################################
dbuildPredict <- function(dt,prm.pt) {
  library(caTools)
  errorFlag1 <- FALSE
  if (sum(!(prm.pt$cpnames %in% colnames(dt))) > 0) {
    cat('\n\nFATAL ERROR in dbuildPredict ... the following requested columns for building predictors are missing...\n')
    cat(prm.pt$cpnames[which(!(prm.pt$cpnames %in% colnames(dt)))])
    errorFlag1 <- TRUE
  }
  if (sum(colnames(dt) %in% prm.pt$timeColName) != 1) {
    cat('\n\nFATAL ERROR in dbuild... time column ', prm.pt$timeColName, ' missing...\n\n')
    errorFlag1 <- TRUE
  }
  # If time column not called 'time', rename it to time
  if (prm.pt$timeColName != 'time') {
    colnames(dt)[colnames(dt) %in% prm.pt$timeColName] <- 'time'
    if (prm.pt$verbose) cat('\nRenaming time column ',prm.pt$timeColName,' to time for random forest training/classification.')
  }
  dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
  # calculates timespan between observations
  dt$timespan <- 0
  dt$timespan[1] <- as.numeric(difftime(dt$time[2],dt$time[1],units='secs'))
  dt$timespan[2:(nrow(dt)-1)] <- as.numeric(difftime(dt$time[3:nrow(dt)],
                                                     dt$time[1:(nrow(dt)-2)],units='secs'))/2
  dt$timespan[nrow(dt)] <- as.numeric(difftime(dt$time[nrow(dt)],
                                               dt$time[(nrow(dt)-1)],units='secs'))
  dt$timespan[dt$timespan<1] <- 1 # minimum allowed timespan value which is used as denominator

  if (!is.null(dt$igbt_temp)) {
    # futureIgbtMax is a calculated target 
    if ('futureIgbtMax' %in% prm.pt$target) {
      dt$futureIgbtMax <- NA
      dt$futureIgbtMax[1:(nrow(dt)-1)] <-
        runmax(dt$igbt_temp[2:nrow(dt)],k=(prm.pt$rowFutureSpan-1),align='left')
      # dt$futureIgbtMax[1:(nrow(dt)-prm.pt$rowFutureOffset)] <-
      #   runmax(dt$igbt_temp[(prm.pt$rowFutureOffset+1):nrow(dt)],k=prm.pt$rowFutureSpan,align='left')
        # runquantile(dt$igbt_temp[(prm.pt$rowFutureOffset+1):nrow(dt)],k=prm.pt$rowFutureSpan,probs=prm.pt$futureQuantile,align='left')
    }
    if ('igbt_tempRunMean' %in% prm.pt$tpnames)
      dt$igbt_tempRunMean <- runmean(dt$igbt_temp,k=prm.pt$rowPastSpan,align='right')
    if ('igbt_tempRunSD' %in% prm.pt$tpnames)
      dt$igbt_tempRunSD <- runsd(dt$igbt_temp,k=prm.pt$rowPastSpan,align='right')
  }

  if (!is.null(dt$block_height)) {
    if ('deltaBlockHeightAbs' %in% prm.pt$tpnames) {
    # if ('igbt_tempRunMean' %in% prm.pt$tpnames) {
      temp <- dt$block_height
      temp[1] <- 0
      temp[2:nrow(dt)] <- abs(dt$block_height[2:nrow(dt)]-dt$block_height[1:(nrow(dt)-1)])
      temp[is.na(temp)] <- 0
      temp <- cumsum(temp)
      if (nrow(dt)>prm.pt$rowPastSpan) {
        dt$deltaBlockHeightAbs <- 0
        dt$deltaBlockHeightAbs[1:prm.pt$rowPastSpan] <- temp[1:prm.pt$rowPastSpan]
        dt$deltaBlockHeightAbs[(prm.pt$rowPastSpan+1):nrow(dt)] <- 
          temp[(prm.pt$rowPastSpan+1):nrow(dt)]-temp[1:(nrow(dt)-prm.pt$rowPastSpan)]
      } else {
        dt$deltaBlockHeightAbs <- temp
      }
    }
  }

  if (!is.null(dt$dc_bus_voltage)) {
    if ('dc_bus_voltageRunMean' %in% prm.pt$tpnames)
      dt$dc_bus_voltageRunMean <- runmean(dt$dc_bus_voltage,k=prm.pt$rowPastSpan,align='right')
    if ('dc_bus_voltageRunSD' %in% prm.pt$tpnames)
      dt$dc_bus_voltageRunSD <- runsd(dt$dc_bus_voltage,k=prm.pt$rowPastSpan,align='right')
  }
  
  if (!is.null(dt$current)) {
    if ('currentRunMean' %in% prm.pt$tpnames)
      dt$currentRunMean <- runmean(dt$current,k=prm.pt$rowPastSpan,align='right')
    if ('currentRunSD' %in% prm.pt$tpnames)
      dt$currentRunSD <- runsd(dt$current,k=prm.pt$rowPastSpan,align='right')
  }
  
  if (!is.null(dt$hookload)) {
    if ('hookloadRunMean' %in% prm.pt$tpnames)
      dt$hookloadRunMean <- runmean(dt$hookload,k=prm.pt$rowPastSpan,align='right')
    if ('hookloadRunSD' %in% prm.pt$tpnames)
      dt$hookloadRunSD <- runsd(dt$hookload,k=prm.pt$rowPastSpan,align='right')
  }

  # Power injected into system
  if (!is.null(dt$current) & !is.null(dt$dc_bus_voltage)) {
    if ('hookloadRunMean' %in% prm.pt$tpnames)
      dt$kwattsRunMean <- runmean((dt$current*dt$dc_bus_voltage/1000),k=prm.pt$rowPastSpan,align='right')
    if ('kwattsRunSD' %in% prm.pt$tpnames)
      dt$kwattsRunSD <- runsd((dt$current*dt$dc_bus_voltage/1000),k=prm.pt$rowPastSpan,align='right')
  }
  
  if (!is.null(dt$frequency)) {
    if ('frequencyRunMean' %in% prm.pt$tpnames)
      dt$frequencyRunMean <- runmean(dt$frequency,k=prm.pt$rowPastSpan,align='right')
    if ('frequencyRunSD' %in% prm.pt$tpnames)
      dt$frequencyRunSD <- runsd(dt$frequency,k=prm.pt$rowPastSpan,align='right')
  }

  if (!is.null(dt$torque)) {
    if ('torqueRunMean' %in% prm.pt$tpnames)
      dt$torqueRunMean <- runmean(dt$torque,k=prm.pt$rowPastSpan,align='right')
    if ('torqueRunSD' %in% prm.pt$tpnames)
      dt$torqueRunSD <- runsd(dt$torque,k=prm.pt$rowPastSpan,align='right')
  }

  if (!is.null(dt$speed)) {
    if ('speedRunMean' %in% prm.pt$tpnames)
      dt$speedRunMean <- runmean(dt$speed,k=prm.pt$rowPastSpan,align='right')
    if ('speedRunSD' %in% prm.pt$tpnames)
      dt$speedRunSD <- runsd(dt$speed,k=prm.pt$rowPastSpan,align='right')
  }

  if (!is.null(dt$output_voltage)) {
    if ('output_voltageRunMean' %in% prm.pt$tpnames)
      dt$output_voltageRunMean <- runmean(dt$output_voltage,k=prm.pt$rowPastSpan,align='right')
    if ('output_voltageRunSD' %in% prm.pt$tpnames)
      dt$output_voltageRunSD <- runsd(dt$output_voltage,k=prm.pt$rowPastSpan,align='right')
  }
  
  # Check that all predictor training variables are present
  if (sum(!(prm.pt$tpnames %in% colnames(dt)))>0) {
    cat('\nFATAL ERROR ... missing the following required predictor variables ...\n')
    cat(prm.pt$tpnames[which(!(prm.pt$tpnames %in% colnames(dt)))])
    stop('FATAL ERROR in dbuild ... missing predictor variables ... ')
  }

  return(dt)
}