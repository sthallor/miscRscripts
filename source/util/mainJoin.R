#######################################################################################
# mainJoin.R - join historian to EDR data with time offset 
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 23, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/get.prm.j.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/util/join.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

cat('\nLoading Historian EDR join parameters from file ',prmFilename,'\n')
prm.j <- get.prm.j(prmFilename)

if (nchar(prm.j$outputName)>0) {
  cat('\nJoining Historian & EDR files and sending output to:\n',prm.j$outputName,'\n')
  sink(file=prm.j$outputName)
  options(width=132)
}

cat(prm.j$version,'\nRun Started at : ',as.character(start.time),'\n')
cat('\nVerbatim listing of offset parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.j$text)) { cat(prm.j$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

# Load the historian datafile
dt.h <- read.csv(prm.j$histName,nrows=-1)
cat('\n\nLoaded Historian file \n', prm.j$histName,' with ',nrow(dt.h),
    ' rows and ',ncol(dt.h),' columns.\n')
# check that essential columns are present
errFlag <- FALSE
if ('time' %in% colnames(dt.h) == FALSE) {
  cat('\n\nERROR in join... Historian file does not have "time" column ...')
  cat('\nBe sure to run dclean on Historian file before running offset ...')
  errFlag <- TRUE
}

# Load the EDR datafile
dt.e <- read.csv(prm.j$EDRName,nrows=-1)

cat('\n\nLoaded EDR file \n', prm.j$EDRName,' with ',nrow(dt.e),
    ' rows and ',ncol(dt.e),' columns.\n')
if ('time' %in% colnames(dt.e) == FALSE) {
  cat('\n\nERROR in join... EDR file does not have "time" column ...')
  cat('\nBe sure to run dclean on EDR file before running offset ...')
  errFlag <- TRUE
}
if (sum(colnames(dt.e) %in% colnames(dt.h)) != 1) {
  cat('\n\nERROR in join... some column names between EDR and Historian match... cannot join...\n')
  errFlag <- TRUE
}

if (errFlag==TRUE) stop('FATAL ERROR in offset program ... missing expected time columns ...')

cat('\nThe earliest Historian date (before offset) is ',as.character(dt.h$time[1]))
cat('\nThe latest Historian date (before offset) is ',as.character(dt.h$time[nrow(dt.h)]))
dt.h$time <- as.POSIXct(as.numeric(as.POSIXlt(dt.h$time,"%Y-%m-%d %H:%M:%S",tz=""))+prm.j$timeOffset,
                        origin="1970-01-01 00:00:00")
dt.h$time <- as.factor(dt.h$time)
cat('\nThe earliest Historian date (with',prm.j$timeOffset,' second offset) is ',as.character(dt.h$time[1]))
cat('\nThe latest Historian date (with',prm.j$timeOffset,' second offset) is ',as.character(dt.h$time[nrow(dt.h)]))
dt.h$timespanAfter <- 0
dt.h$timespanAfter[1:(nrow(dt.h)-1)] <- as.numeric(difftime(dt.h$time[2:nrow(dt.h)],
                                                            dt.h$time[1:(nrow(dt.h)-1)],units='secs'))
cat('\nThe minimum Historian interval timespan is ',min(dt.h$timespanAfter[1:(nrow(dt.h)-1)]),' seconds.')
medianTimespanH <- median(dt.h$timespanAfter[1:(nrow(dt.h)-1)])
cat('\nThe median  Historian interval timespan is ',medianTimespanH,' seconds.')
cat('\nThe maximum Historian interval timespan is ',max(dt.h$timespanAfter[1:(nrow(dt.h)-1)]),' seconds.\n')
dt.h$timespanAfter <- NULL

cat('\nThe earliest EDR date is ',as.character(dt.e$time[1]))
cat('\nThe latest EDR date is ',as.character(dt.e$time[nrow(dt.e)]))
dt.e$timespanAfter <- 0
# dt.e$timespanAfter[1:(nrow(dt.e)-1)] <- as.numeric(difftime(as.POSIXlt(dt.e$time[2:nrow(dt.e)],"%Y-%m-%d %H:%M:%S",tz=""),
#                                                             as.POSIXlt(dt.e$time[1:(nrow(dt.e)-1)],"%Y-%m-%d %H:%M:%S",tz=""),units='secs'))
dt.e$timespanAfter[1:(nrow(dt.e)-1)] <- as.numeric(difftime(dt.e$time[2:nrow(dt.e)],
                                                            dt.e$time[1:(nrow(dt.e)-1)],units='secs'))
cat('\nThe minimum EDR interval timespan is ',min(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.')
medianTimespanE <- median(dt.e$timespanAfter[1:(nrow(dt.e)-1)])
cat('\nThe median  EDR interval timespan is ',median(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.')
cat('\nThe maximum EDR interval timespan is ',max(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.\n')
dt.h$timespanAfter <- NULL

# Merge and sort the historian and EDR datasets 
if (medianTimespanH != medianTimespanE) {
  cat('\n\nFATAL ERROR ... median EDR timespan=',medianTimespanE,
      ' not equal to median Historian timespan=',medianTimespanH)
  stop('join FATAL ERROR ... EDR and Historian timespans not compatiable ...')
}

dt <- merge(dt.h,dt.e,by='time',all=TRUE)
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
dt <- dt[order(dt$time),]
cat('\n\nCreated historian and EDR merged dataset with ',nrow(dt),' rows and ',ncol(dt),' columns.')
cat('\nThere were ',sum(dt.e$time %in% dt.h$time),' matching EDR and Historian rows.')
cat('\nThe earliest combined date is ',as.character(dt$time[1]))
cat('\nThe latest combined date is ',as.character(dt$time[nrow(dt)]))

cat('\n\nWriting file \n', prm.j$joinName,'\n')
write.csv(dt,file=prm.j$joinName,row.names=FALSE)

stop.time <- Sys.time()
cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')
cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
