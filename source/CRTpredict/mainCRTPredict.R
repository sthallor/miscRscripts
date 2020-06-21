#######################################################################################
# Real Time C Predict V3.0 - R code to immulate C code calling R predict functions
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 10, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)

# Declare some variables
start.time <- Sys.time() # Used for terminating while loop
rtp <- list()            # List for passing data and results to/from R functions    
results <- NULL          # Accumulates results in mainline routine only 
rtp$sourceDir <- "C:/Users/Fred.Seymour/Rcode/master/" # Source code directory absolute address

library(RODBC)
source(paste0(rtp$sourceDir,"source/CRTpredict/rtpInit.R"))
source(paste0(rtp$sourceDir,"source/CRTpredict/rtpTest.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  rtp$prmFilename <- arguments[1]
} else {
  rtp$prmFilename <- paste0(rtp$sourceDir,'parms/dev/CRTpredict/CRTpredict.prm')
  cat("\nNo command line argument ... using default parameters filename ",rtp$prmFilename,"\n\n")
}

#########################################################################
# Call to R function to Initialize list of real time prediction variables
# rtp list contains the following:
# rtp$sourceDir = path to top R source code directory
# rtp$prmFilename = path plus filename for RTpredict parameters file
rtp <- rtpInit(rtp)
#########################################################################

# Output text sent to console or log file
if (nchar(rtp$prm.rp$outputLogFilename)>0) {
  sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
  options(width=132)
}
#########################################################################
# Opens SQL connection - C equivalent needed here
rtp$prm.rp$ODBCConnectName <- 'rig140'
rtp$prm.rp$SQLqueryBufferTime <- 100 # seconds
rtp$prm.rp$lagDelay <- 25
cat('\nOpening SQL connection with ',rtp$prm.rp$ODBCConnectName)
ODBC_channel <- odbcConnect(rtp$prm.rp$ODBCConnectName)
cat('\nOpened RODBC Connection=',ODBC_channel,'   Sys.time()=',as.character(Sys.time()),'\n')
#########################################################################
if (sink.number()>0) sink(file=NULL)  # return output to console if log file was selected

elapsedTime <- 0 # start with zero elapsed time

# Loop for elapsed time duration of real time testing
while (elapsedTime < rtp$prm.rp$elapsedtime) {
  process.start.time <- Sys.time()
  
  # Output text sent to console or log file
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
    options(width=132)
  }

  startEpoch <- round(as.numeric(process.start.time) - rtp$prm.rp$lagDelay -
                        rtp$prm.pt$rowPastSpan*rtp$prm.pt$dominantTimeInterval -
                        rtp$prm.rp$SQLqueryBufferTime)*1000
  endEpoch <- round(as.numeric(process.start.time))*1000
  
  # builds sql query from startEpoch and endEpoch
  sql_query <- paste0("CALL `get_igbt_data_between_two_dates`(",
                      as.character(startEpoch),",",as.character(endEpoch),")")
  if (rtp$prm.rp$verbose) cat('\nODBC_channel=',ODBC_channel,' sql_query=',sql_query)
  tags <- sqlQuery(ODBC_channel,sql_query)
  
  if (class(tags)=='data.frame') {
    if (rtp$prm.rp$verbose) cat('\nsql_query returned tags dataframe with ',
                                nrow(tags),' rows and ',ncol(tags),' cols.')
    # Converts epoch time stamp into POSIXlt
    tags[[rtp$prm.dc$timeColName]] <- as.POSIXlt(tags[[rtp$prm.dc$timeColName]]/1000,origin="1970-01-01 00:00:00")
    
    # Delete any records that are within rtp$prm.rp$lagDelay of process.start.time
    if (rtp$prm.rp$verbose) cat('\nDeleting ',sum(as.numeric(difftime(process.start.time,tags[[rtp$prm.dc$timeColName]],units='secs'))<rtp$prm.rp$lagDelay),
                                ' sql tag records within rtp$prm.rp$lagDelay=',rtp$prm.rp$lagDelay,' of current time=',as.character(process.start.time))
    tags <- tags[as.numeric(difftime(process.start.time,tags[[rtp$prm.dc$timeColName]],units='secs'))>=rtp$prm.rp$lagDelay,]

    #########################################################################
    # Call to R function to make prediction from real time data
    # rtp list going in contains the following:
    # rtp$sourceDir = path to top R source code directory
    # rtp$prmFilename = path plus filename for RTpredict parameters file
    # rtp$prm.rp = parameters for real time prediction
    # rtp$prm.pt = parameters for random forest prediction train
    # rtp$prm.dc = parameters for data clean
    # rtp$rf = random forest model
    # coming out the list additionally contains:
    # rtp$dt.output = prediction output each record has (time, epoch, obsCount, igbt_tempRunMean, 
    #     rfTempPredict, rfGTcut, alarm, processTime, lagTime)
    # rtp$dt.diag = all of the variables used in the model plus alarm prediction
    # rtp$dt.all = input variables after dclean and before dbuild 
    
    rtp <- rtpTest(tags,rtp)

    results <- if (is.null(results)) rtp$dt.output else rbind(results,rtp$dt.output)
    #########################################################################
  # Error trap in case of bad SQL read  
  } else { # end if (class(tag)=="data.frame")
    if (rtp$prm.rp$verbose) {
      cat('\nProblems with sql query\n')
      print(tags)
    }
    rtp$errorCount = rtp$errorCount + 1
  }

  Sys.sleep(rtp$prm.rp$sleeptime)
  elapsedTime <- as.numeric(difftime(Sys.time(),start.time,units='hours'))
  if (sink.number()>0) {
    sink(file=NULL)  # return output to console if log file was selected
    cat('\nConsole: time=',rtp$dt.output$time,' igbt_tempRunMean=',rtp$dt.output$igbt_tempRunMean,' alarm=',rtp$dt.output$alarm) # to console
  }
} # end of while loop

# Output text sent to console or log file
if (nchar(rtp$prm.rp$outputLogFilename)>0) {
  sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
  options(width=132)
}

cat('\nResults from run:\n')
print(results)

# Write prediction .csv file if requested (non-blank filename)
if (nchar(rtp$prm.rp$outputResultFilename) >0 & nrow(rtp$dt.output)>0) {
  cat('\nWriting RTpredict results file with ',nrow(results), 'rows and ',ncol(results), 
      ' columns (csv file) : \n',rtp$prm.rp$outputResultFilename,'\n')
  write.csv(results,
            file=rtp$prm.rp$outputResultFilename,row.names=FALSE)
}
# write rtp$dt.diag file for Diagnostics
if (nchar(rtp$prm.rp$outputDiagFilename)>0) {
  cat('\nWriting RTpredict diagnostics file with ',nrow(rtp$dt.diag), 'rows and ',ncol(rtp$dt.diag), 
      ' columns (csv file) : \n',rtp$prm.rp$outputDiagFilename,'\n')
  write.csv(rtp$dt.diag,
            file=rtp$prm.rp$outputDiagFilename,row.names=FALSE)
}

cat('\nClosing RODBC Connetion=',ODBC_channel,'   Sys.time()=',as.character(Sys.time()),'\n')
close(ODBC_channel)

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3),' hours.\n')

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)  # return output to console if log file was selected