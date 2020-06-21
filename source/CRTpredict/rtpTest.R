#######################################################################################
# rtpTest.R - testing routine for real time predictions callable from C
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 10, 2017
#######################################################################################
rtpTest <- function(tags,rtp) {
  # Load required libraries and functions
  library(randomForest)
  source(paste0(rtp$sourceDir,"source/CRTpredict/dbuildPredict.R"))
  source(paste0(rtp$sourceDir,"source/CRTpredict/impute.R"))     # for dclean
  source(paste0(rtp$sourceDir,"source/CRTpredict/dclean.R"))     # for dclean
  source(paste0(rtp$sourceDir,"source/CRTpredict/reshape.R"))    # for dclean
  
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
    options(width=132)
  }
    
  # If option selected, performs dclean 
  if (nchar(rtp$prm.rp$dcleanPrmFilename)>0) {
    if (rtp$prm.dc$reshapeHistorianData==TRUE) {
      rtp$dt.all <- reshape(tags,rtp$prm.dc)
    } else {
      rtp$dt.all <- tags
    }
    rtp$dt.all <- dclean(rtp$dt.all,rtp$prm.dc)
  } else {
    rtp$dt.all <- tags
  }
      
  if (rtp$prm.rp$verbose) cat('\nBuilding predictor feature variables.')
  dtb <- tryCatch({
    dbuildPredict(rtp$dt.all,prm.pt=rtp$prm.pt)
  },
  error=function(e) {
    cat('\n\nERRORS in dbuildPredict, UNABLE TO PERFORM PREDICTIONS:\n',e$message,'\n\n')
    dtb <- NULL
  })
      
  if (!is.null(dtb)) { # Only continue processing if dbuild was successful
    dtb <- dtb[,c('time',rtp$prm.pt$cpnames,rtp$prm.pt$tpnames)]
    dtb$time <- as.character(dtb$time) # Needed for complete.cases to work
    dtb <- dtb[complete.cases(dtb),]
    if (rtp$prm.rp$verbose) cat('\nPredictor variables with complete cases ',nrow(dtb),' rows and ',ncol(dtb),' columns.')
    obsCount <- nrow(dtb)
    dtb <- dtb[nrow(dtb),] # Only process latest observations
    rf.all <- predict(rtp$rf,dtb,predict.all=TRUE)
    dtb$predicted[1] <- rf.all$aggregate[1]
    dtb$rfGTcut[1] <- sum(rf.all$individual>rtp$prm.pt$targetAlarmCutoff)/ncol(rf.all$individual)
    dtb$rfAlarm <- sum(rf.all$individual>rtp$prm.pt$targetAlarmCutoff)/ncol(rf.all$individual)>=rtp$prm.rp$rfGTCutoffAlarm

    rtp$dt.output <- data.frame(time=as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""),
                                epoch=as.character(as.numeric(as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""))*1000),
                                obsCount=obsCount,
                                igbt_tempRunMean=round(dtb$igbt_tempRunMean[1],digits=2),
                                rfTempPredict=round(dtb$predicted[1],digits=2),
                                rfGTcut=round(dtb$rfGTcut[1],digits=3),
                                alarm=dtb$rfAlarm[1],
                                lagTime=round(as.numeric(difftime(Sys.time(),as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""),
                                                                            units='secs')),digits=1))
    # Write results to log file
    cat('\ntime=',rtp$dt.output$time,' igbt_tempRunMean=',rtp$dt.output$igbt_tempRunMean,' alarm=',rtp$dt.output$alarm)
    # if (rtp$prm.rp$verbose) cat('\n\n')
   
    # Build diagnostics file if requested
    if (nchar(rtp$prm.rp$outputDiagFilename)>0) {
      rtp$dt.diag <- rbind(rtp$dt.diag,dtb)
    }
  } else {
    cat('\nERROR in dbuild\n')
  }
        
  if (sink.number()>0) sink(file=NULL)
  return(rtp)
}