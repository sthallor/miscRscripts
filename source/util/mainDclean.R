#######################################################################################
# mainDclean.R - standalone programe to clean one or more .csv files 
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 23, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)

# Source code master directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/get.prm.dc.R"))
source(paste0(sourceDir,"source/util/impute.R"))
source(paste0(sourceDir,"source/util/dclean.R"))
source(paste0(sourceDir,"source/util/reshape.R"))

###############################################################################################
# Processing begins here

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/util/dclean.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

cat('\nLoading dclean parameters from file ',prmFilename,'\n')
prm.dc <- get.prm.dc(prmFilename)

# Loop through the files to be cleaned
for (f in 1:length(prm.dc$inputNames)) {
  if (sink.number()>0) sink(file=NULL)
  start.time <- Sys.time()
  if (nchar(prm.dc$logNames[f])>0) {
    cat('\nCleaning data files and sending output to:\n',prm.dc$logNames[f],'\n')
    sink(file=prm.dc$logNames[f])
    options(width=132)
  }

  cat('\n',prm.dc$version)
  cat('\nRun Started at : ',as.character(start.time),'\n')
  cat('\nVerbatim listing of offset parameters file from:\n',prmFilename,'\n\n')
  cat(paste(rep('-',80),collapse=''),'\n')
  for (i in 1:length(prm.dc$text)) { cat(prm.dc$text[i],'\n') }
  cat(paste(rep('-',80),collapse=''),'\n')

  # Load the .csv file to be cleaned
  dt <- read.csv(prm.dc$inputNames[f],nrows=-1)

  cat('\n\nLoaded file \n', prm.dc$inputNames[f],'\n with ',nrow(dt),
      ' rows and ',ncol(dt),' columns.\n')

  if (prm.dc$reshapeHistorianData==TRUE) {
    dt <- reshape(dt,prm.dc)
  }

  dt <- dclean(dt,prm.dc)

  cat('\n\nWriting file \n', prm.dc$outputNames[f],'\n with ',nrow(dt),
      ' rows and ',ncol(dt),' columns.\n')
  write.csv(dt,file=prm.dc$outputNames[f],row.names=FALSE)
  
  stop.time <- Sys.time()
  cat('\n\nProgram Execution Elapsed time ', 
      round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')
}
if (sink.number()>0) sink(file=NULL)
