#######################################################################################
# Real Time Predict V3.0 - main routine for target prediction
# Ensign Energy Services Inc. retains all rights to this software
# FHS, June 2, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

library(RODBC)
library(randomForest)
 
source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/predict/get.prm.RTpredict.R"))
source(paste0(sourceDir,"source/predict/get.prm.predictTrain.R"))
source(paste0(sourceDir,"source/predict/dbuildPredict.R"))
source(paste0(sourceDir,"source/predict/rtpInit.R"))
source(paste0(sourceDir,"source/predict/rtpTest.R"))
source(paste0(sourceDir,"source/predict/rtpEnd.R"))
source(paste0(sourceDir,"source/util/get.prm.dc.R")) # for dclean
source(paste0(sourceDir,"source/util/impute.R"))     # for dclean
source(paste0(sourceDir,"source/util/dclean.R"))     # for dclean
source(paste0(sourceDir,"source/util/reshape.R"))    # for dclean
 
# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/predict/RTpredict.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Initialize list of real time prediction variables
rtp <- rtpInit(prmFilename)

cat('\nOpening SQL connection with ',rtp$prm.rp$ODBCConnectName)
rtp$ch <- odbcConnect(rtp$prm.rp$ODBCConnectName)
cat('\nOpened RODBC Connection=',rtp$ch,'   Sys.time()=',as.character(Sys.time()),'\n')

elapsedTime <- 0 # start with zero elapsed time

while (elapsedTime < rtp$prm.rp$elapsedtime) {
  process.start.time <- Sys.time()
  
  rtp <- rtpTest(process.start.time,rtp)

  Sys.sleep(rtp$prm.rp$sleeptime)
  elapsedTime <- as.numeric(difftime(Sys.time(),start.time,units='hours'))
} # end of while loop

rtp <- rtpEnd(rtp)

cat('\nClosing RODBC Connetion=',rtp$ch,'   Sys.time()=',as.character(Sys.time()),'\n')
close(rtp$ch)

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3),' hours.\n')

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
