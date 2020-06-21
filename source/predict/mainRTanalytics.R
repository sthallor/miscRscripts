#######################################################################################
# Real Time Analytics V3.0 - main routine for rig state classification and predictions
# Ensign Energy Services Inc. retains all rights to this software
# FHS, June 5, 2017
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
source(paste0(sourceDir,"source/predict/get.prm.RTpredict.R"))
source(paste0(sourceDir,"source/predict/get.prm.predictTrain.R"))
source(paste0(sourceDir,"source/predict/dbuildPredict.R"))
source(paste0(sourceDir,"source/predict/rtpInit.R"))
source(paste0(sourceDir,"source/predict/rtpTest.R"))
source(paste0(sourceDir,"source/predict/rtpEnd.R"))
source(paste0(sourceDir,"source/util/runmeanA.R"))
source(paste0(sourceDir,"source/util/slope.R"))
source(paste0(sourceDir,"source/util/ftd.R"))
source(paste0(sourceDir,"source/util/get.prm.dc.R")) # for dclean
source(paste0(sourceDir,"source/util/impute.R"))     # for dclean
source(paste0(sourceDir,"source/util/dclean.R"))     # for dclean
source(paste0(sourceDir,"source/util/reshape.R"))    # for dclean

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)

# initialize real time classification parameters and model
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/classify/RTclassify.prm')
  cat("\nNo command line argument ... using default classify parameters filename ",prmFilename,"\n\n")
}
rtc <- rtcInit(prmFilename)

# Initialize list of real time prediction variables
if (length(arguments)==1) {
  prmFilename <- gsub('RTclassify.prm','RTpredict.prm',prmFilename)
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/predict/RTpredict.prm')
  cat("\nNo command line argument ... using default predict parameters filename ",prmFilename,"\n\n")
}
rtp <- rtpInit(prmFilename)

cat('\nOpening SQL connection with ',rtc$prm.rt$ODBCConnectName,'\n')
rtc$ch <- odbcConnect(rtc$prm.rt$ODBCConnectName)
rtp$ch <- rtc$ch
cat('\nOpenned RODBC Connect=',rtc$ch,'   Sys.time()=',as.character(Sys.time()),'\n')

elapsedTime <- 0 # start with zero elapsed time

while (elapsedTime < rtc$prm.rt$elapsedtime) {
  process.start.time <- Sys.time()
  
  rtc <- rtcTest(process.start.time,rtc)
  process.time <- as.numeric(difftime(Sys.time(),process.start.time,units='secs'))
#   cat('\n\nDIAG timestamp=',as.character(process.start.time),' Cum Process time after rtcTest=',round(process.time,digits=1),
#       ' nrow=',nrow(rtc$dt.output),' last timestamp=',as.character(rtc$dt.output$time[nrow(rtc$dt.output)]))
  rtp <- rtpTest(process.start.time,rtp)
  process.time <- as.numeric(difftime(Sys.time(),process.start.time,units='secs'))
#   cat('\nDIAG timestamp=',as.character(process.start.time),' Cum Process time after rtpTest=',round(process.time,digits=1),
#       ' nrow=',nrow(rtp$dt.output),' last timestamp=',as.character(rtp$dt.output$time[nrow(rtp$dt.output)]))
  if (process.time < rtc$prm.rt$sleeptime) Sys.sleep((rtc$prm.rt$sleeptime-process.time))
  elapsedTime <- as.numeric(difftime(Sys.time(),start.time,units='hours'))
} # end of while loop

rtc <- rtcEnd(rtc)

rtp <- rtpEnd(rtp)

cat('\nClosing RODBC Connetion=',rtc$ch,'   Sys.time()=',as.character(Sys.time()),'\n')
close(rtc$ch)

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3),' hours.\n')

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
