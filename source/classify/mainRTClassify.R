#######################################################################################
# Real Time Rig Classify V3.0 - main routine for rig state classification testing
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 28, 2019
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

library(RODBC)
library(randomForest)

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/classify/get.prm.t.R"))
source(paste0(sourceDir,"source/classify/dbuild.R"))
source(paste0(sourceDir,"source/classify/dreport.R"))
source(paste0(sourceDir,"source/classify/postprocess.R"))
source(paste0(sourceDir,"source/classify/get.prm.rt.R"))
source(paste0(sourceDir,"source/classify/rtcInit.R"))
source(paste0(sourceDir,"source/classify/rtcTest.R"))
source(paste0(sourceDir,"source/classify/rtcEnd.R"))
source(paste0(sourceDir,"source/util/runmeanA.R"))
source(paste0(sourceDir,"source/util/slope.R"))
source(paste0(sourceDir,"source/util/ftd.R"))
source(paste0(sourceDir,"source/util/get.prm.dc.R")) # for dclean
source(paste0(sourceDir,"source/util/impute.R"))     # for dclean
source(paste0(sourceDir,"source/util/dclean.R"))     # for dclean
source(paste0(sourceDir,"source/util/reshape.R"))    # for dclean

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/classify/RTclassify.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# initialize real time classification parameters and model
rtc <- rtcInit(prmFilename)

# Output to log file if requested
if (nchar(rtc$prm.rt$outputLogFilename)>0) {
  sink(file=rtc$prm.rt$outputLogFilename, append=TRUE)
  options(width=132)
}

print(sessionInfo())

cat('\nOpening SQL connection with ',rtc$prm.rt$ODBCConnectName,'\n')
rtc$ch <- odbcConnect(rtc$prm.rt$ODBCConnectName)
cat('\nOpenned RODBC Connect=',rtc$ch,'   Sys.time()=',as.character(Sys.time()),'\n')

rtc$elapsedTime <- 0 # start with zero elapsed time

while (rtc$elapsedTime < rtc$prm.rt$elapsedtime) {
  process.start.time <- Sys.time()
  
  rtc <- rtcTest(process.start.time,rtc)

  Sys.sleep(rtc$prm.rt$sleeptime)
  rtc$elapsedTime <- as.numeric(difftime(Sys.time(),start.time,units='hours'))
  
  # Write running log before end of elapsed time if criteria met
  if (rtc$prm.rt$earlySaveProportion<1 & !rtc$prm.rt$earlySaveFlag) {
    # Option to write running log before end of elapsed time selected and not already saved.
    if (rtc$elapsedTime/rtc$prm.rt$elapsedtime > rtc$prm.rt$earlySaveProportion & rtc$elapsedTime/rtc$prm.rt$elapsedtime < 1) {
      # Criteria met, proceed with early write of running log
      rtc <- rtcRunLogWrite(rtc,start.time)
    }
  }
} # end of while loop

rtc <- rtcRunLogWrite(rtc,start.time)
rtc <- rtcEnd(rtc)

cat('\nClosing RODBC Connection=',rtc$ch,'   Sys.time()=',as.character(Sys.time()),'\n')
close(rtc$ch)

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3),' hours.\n')

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
