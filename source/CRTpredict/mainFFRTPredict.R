#######################################################################################
# Real Time Predict V3.0 - R code for Flat File handshake with C code for RTPredict
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 12, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)

# Declare some variables
start.time <- Sys.time() # Used for terminating while loop
rtp <- list()            # List for passing data and results to/from R functions    
results <- NULL          # Accumulates results in mainline routine only 

rtp$sourceDir <- "C:/Users/Fred.Seymour/Rcode/master/" # Source code directory absolute address
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

# Insure that .csv input and output filenames exist
if (rtp$prm.rp$inputFilename=='') {
  cat('\nERROR - must have valid .csv input filename parameter for Flat File handshake RTPredict to run')
  stop("No .csv input filename for Flat File handshake")
}
if (rtp$prm.rp$outputFilename=='') {
  cat('\nERROR - must have valid .csv output filename parameter for Flat File handshake RTPredict to run')
  stop("No .csv output filename for Flat File handshake")
}

# Output text sent to console or log file
if (nchar(rtp$prm.rp$outputLogFilename)>0) {
  sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
  options(width=132)
}

if (sink.number()>0) sink(file=NULL)  # return output to console if log file was selected

elapsedTime <- 0 # start with zero elapsed time
# Initialize previous.file.mtime
# previous.file.mtime <- as.numeric(file.mtime(rtp$prm.rp$inputFilename)) - 10
previous.file.mtime <- as.numeric(Sys.time())


# Loop for elapsed time duration of real time testing
while (elapsedTime < rtp$prm.rp$elapsedtime) {
  process.start.time <- Sys.time()
  
  # Output text sent to console or log file
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
    options(width=132)
  }

  # Look for new instance of flat file handshake input .csv file
  if (file.exists(rtp$prm.rp$inputFilename)) {
    current.file.mtime <- as.numeric(file.mtime(rtp$prm.rp$inputFilename))
    if (current.file.mtime > previous.file.mtime) {
      tags <- read.csv(rtp$prm.rp$inputFilename)
      if (rtp$prm.rp$verbose) {
        cat('\nJust read input file ',rtp$prm.rp$inputFilename)
        cat('\nwith ',nrow(tags),' rows and ',ncol(tags),' columns.')
        cat('\ncurrent.file.mtime=',current.file.mtime,' previous.file.mtime=',previous.file.mtime)
      }
      previous.file.mtime <- current.file.mtime
    } else {
      tags <- NULL
    }
  }
  
  if (class(tags)=='data.frame') {
    if (rtp$prm.rp$verbose) cat('\nloaded flat file handshake dataframe with ',
                                nrow(tags),' rows and ',ncol(tags),' cols.')
    # Converts epoch time stamp into POSIXlt
    tags[[rtp$prm.dc$timeColName]] <- as.POSIXlt(tags[[rtp$prm.dc$timeColName]]/1000,origin="1970-01-01 00:00:00")
    t1 <- 
    if (rtp$prm.rp$verbose) cat('\nPredictor data times span from ',
                                as.character(min(tags[[rtp$prm.dc$timeColName]])),' to ',
                                as.character(max(tags[[rtp$prm.dc$timeColName]])))

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
    
    write.csv(rtp$dt.output,
              file=rtp$prm.rp$outputFilename,row.names=FALSE)
    if (rtp$prm.rp$verbose) cat('\nWrite prediction to flat file handshake output .csv file with ',
                                nrow(rtp$dt.output),' rows and ',ncol(rtp$dt.output),' cols.\n\n')
    
    #########################################################################
  # Error trap in case of bad data read  
  } else { # end if (class(tag)=="data.frame")
    if (rtp$prm.rp$verbose) {
      if (is.null(tags)) {
        cat('\nAs of time=',as.numeric(Sys.time()),' flat file handshake input .csv file timestamp unchanged at ',current.file.mtime)
      } else {
        cat('\nProblems with flat file handshake input data\n')
        print(tags)
      }
    }
    rtp$errorCount = rtp$errorCount + 1
  }

  Sys.sleep(rtp$prm.rp$sleeptime)
  elapsedTime <- as.numeric(difftime(Sys.time(),start.time,units='hours'))
  if (sink.number()>0) {
    sink(file=NULL)  # return output to console if log file was selected
    # cat('\nConsole: time=',rtp$dt.output$time,' igbt_tempRunMean=',rtp$dt.output$igbt_tempRunMean,' alarm=',rtp$dt.output$alarm) # to console
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

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3),' hours.\n')

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)  # return output to console if log file was selected