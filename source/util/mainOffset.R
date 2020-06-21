#######################################################################################
# mainOffset.R - time offset to max correlation between EDR & Hist block heights 
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Nov 27, 2017
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/get.prm.os.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/util/offset.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

cat('\nLoading Historian EDR time offset parameters from file ',prmFilename,'\n')
prm.os <- get.prm.os(prmFilename)

if (nchar(prm.os$outputName)>0) {
  cat('\nCalculating offset and sending output to:\n',prm.os$outputName,'\n')
  sink(file=prm.os$outputName)
  options(width=132)
}

cat(prm.os$version,'\nRun Started at : ',as.character(start.time),'\n')
cat('\nVerbatim listing of offset parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.os$text)) { cat(prm.os$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

# Load the historian datafile
dt.h <- read.csv(prm.os$histName,nrows=-1)
cat('\n\nLoaded Historian file \n', prm.os$histName,' with ',nrow(dt.h),
    ' rows and ',ncol(dt.h),' columns.\n')
# check that essential columns are present
errFlag <- FALSE
if ('time' %in% colnames(dt.h) == FALSE) {
  cat('\n\nERROR in offset... Historian file does not have "time" column ...')
  cat('\nBe sure to run dclean on Historian file before running offset ...')
  errFlag <- TRUE
}
if (prm.os$histKeyword %in% colnames(dt.h) == FALSE) {
  cat('\n\nERROR in offset... Historian file does not have ',prm.os$histKeyword,' column ...')
  cat('\nPlease check parameters and try again ...')
  errFlag <- TRUE
}

# Load the EDR datafile
dt.e <- read.csv(prm.os$EDRName,nrows=-1)

cat('\n\nLoaded EDR file \n', prm.os$EDRName,' with ',nrow(dt.e),
    ' rows and ',ncol(dt.e),' columns.\n')
if ('time' %in% colnames(dt.e) == FALSE) {
  cat('\n\nERROR in offset... EDR file does not have "time" column ...')
  cat('\nBe sure to run dclean on EDR file before running offset ...')
  errFlag <- TRUE
}
if (prm.os$EDRKeyword %in% colnames(dt.e) == FALSE) {
  cat('\n\nERROR in offset... EDR file does not have ',prm.os$EDRKeyword,' column ...')
  cat('\nPlease check parameters and try again ...')
  errFlag <- TRUE
}

if (errFlag==TRUE) stop('FATAL ERROR in offset program ... missing expected columns ...')

dt.h <- dt.h[,c('time',prm.os$histKeyword)] # only keep columns needed for offset calc

dt.h[[prm.os$histKeyword]] <- as.numeric(as.character(dt.h[[prm.os$histKeyword]]))
cat('\n\n',prm.os$histKeyword,' Minimum value=',min(dt.h[[prm.os$histKeyword]],na.rm=T),
    ' Maximum value=',max(dt.h[[prm.os$histKeyword]],na.rm=T))
cat('\nThe earliest Historian ',prm.os$histKeyword,' date is ',as.character(dt.h$time[1]))
cat('\nThe latest Historian ',prm.os$histKeyword,' date is ',as.character(dt.h$time[nrow(dt.h)]))
dt.h$timespanAfter <- 0
dt.h$timespanAfter[1:(nrow(dt.h)-1)] <- as.numeric(difftime(dt.h$time[2:nrow(dt.h)],
                                                            dt.h$time[1:(nrow(dt.h)-1)],units='secs'))
cat('\nThe minimum Historian interval timespan is ',min(dt.h$timespanAfter[1:(nrow(dt.h)-1)]),' seconds.')
medianTimespanH <- median(dt.h$timespanAfter[1:(nrow(dt.h)-1)])
cat('\nThe median  Historian interval timespan is ',medianTimespanH,' seconds.')
cat('\nThe maximum Historian interval timespan is ',max(dt.h$timespanAfter[1:(nrow(dt.h)-1)]),' seconds.\n')
dt.h$timespanAfter <- NULL

dt.e <- dt.e[,c('time',prm.os$EDRKeyword)] # only keep columns needed for offset calc

dt.e[[prm.os$EDRKeyword]] <- as.numeric(as.character(dt.e[[prm.os$EDRKeyword]]))
cat('\n\n',prm.os$EDRKeyword,' Minimum value=',min(dt.e[[prm.os$EDRKeyword]],na.rm=T),
    ' Maximum value=',max(dt.e[[prm.os$EDRKeyword]],na.rm=T))
cat('\nThe earliest EDR ',prm.os$EDRKeyword,' date is ',as.character(dt.e$time[1]))
cat('\nThe latest EDR ',prm.os$EDRKeyword,' date is ',as.character(dt.e$time[nrow(dt.e)]))
dt.e$timespanAfter <- 0
dt.e$timespanAfter[1:(nrow(dt.e)-1)] <- as.numeric(difftime(dt.e$time[2:nrow(dt.e)],
                                                            dt.e$time[1:(nrow(dt.e)-1)],units='secs'))
cat('\nThe minimum EDR interval timespan is ',min(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.')
medianTimespanE <- median(dt.e$timespanAfter[1:(nrow(dt.e)-1)])
cat('\nThe median  EDR interval timespan is ',median(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.')
cat('\nThe maximum EDR interval timespan is ',max(dt.e$timespanAfter[1:(nrow(dt.e)-1)]),' seconds.\n')
dt.h$timespanAfter <- NULL

# Drop any records that are more than min/max offset beyond respective min/max time values
# Look at minimums first
if(difftime(dt.e$time[1],dt.h$time[1],units='secs')<0) {
  # There are EDR times before first Historian time
  if (sum(difftime(dt.e$time,dt.h$time[1],units='secs')<(prm.os$offsetMin*medianTimespanH))>0) {
    cat('\nTruncating ',sum(difftime(dt.e$time,dt.h$time[1],units='secs')<(prm.os$offsetMin * medianTimespanH)),
        ' EDR readings more than ',(-prm.os$offsetMin*medianTimespanH),
        ' seconds before first ', prm.os$histKeyword,' reading')
    dt.e <- dt.e[difftime(dt.e$time,dt.h$time[1],units='secs')>=(prm.os$offsetMin * medianTimespanH),]
  }
} else {
  # There are Historian times before EDR times
  if (sum(difftime(dt.e$time[1],dt.h$time,units='secs')>(prm.os$offsetMax*medianTimespanH))>0) {
    cat('\nTruncating ',sum(difftime(dt.e$time[1],dt.h$time,units='secs')>(prm.os$offsetMax*medianTimespanH)),
        ' Historian readings more than ',(prm.os$offsetMax*medianTimespanH),
        ' seconds before first ', prm.os$EDRKeyword,' reading')
    dt.h <- dt.h[difftime(dt.e$time[1],dt.h$time,units='secs')<=(prm.os$offsetMax*medianTimespanH),]
  }
}
# Now look at maximums
if(difftime(dt.e$time[nrow(dt.e)],dt.h$time[nrow(dt.h)],units='secs')>0) {
  # There are EDR times after last Historian time
  if (sum(difftime(dt.e$time,dt.h$time[nrow(dt.h)],units='secs')>(prm.os$offsetMax*medianTimespanH))>0) {
    cat('\nTruncating ',sum(difftime(dt.e$time,dt.h$time[nrow(dt.h)],units='secs')>(prm.os$offsetMax*medianTimespanH)),
        ' EDR readings more than ',(prm.os$offsetMax*medianTimespanH),
        ' seconds after last ', prm.os$histKeyword,' reading')
    dt.e <- dt.e[difftime(dt.e$time,dt.h$time[nrow(dt.h)],units='secs')<=(prm.os$offsetMax*medianTimespanH),]
  }
} else {
  # There are Historian times after the last EDR time
  if (sum(difftime(dt.e$time[nrow(dt.e)],dt.h$time,units='secs')<(prm.os$offsetMin*medianTimespanH))>0) {
    cat('\nTruncating ',sum(difftime(dt.e$time[nrow(dt.e)],dt.h$time,units='secs')<(prm.os$offsetMin*medianTimespanH)),
        ' Historian readings more than ',(-prm.os$offsetMin*medianTimespanH),
        ' seconds after last ', prm.os$EDRKeyword,' reading')
    dt.h <- dt.h[difftime(dt.e$time[nrow(dt.e)],dt.h$time,units='secs')>=(prm.os$offsetMin*medianTimespanH),]
  }
}

# Merge and sort the historian and EDR datasets prior to looking at time offsets
if (medianTimespanH != medianTimespanE) {
  cat('\n\nFATAL ERROR ... median EDR timespan=',medianTimespanE,
      ' not equal to median Historian timespan=',medianTimespanH)
  stop('offset FATAL ERROR ... EDR and Historian timespans not compatiable ...')
}
dt <- merge(dt.h,dt.e,by='time',all=TRUE)
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
dt <- dt[order(dt$time),]
cat('\n\nCreated historian and EDR merged dataset with ',nrow(dt),' rows and ',ncol(dt),' columns.')
cat('\nThere are ',sum(!(is.na(dt[,2]))),prm.os$histKeyword,' data points and ',
    sum(is.na(dt[,2])),' missing.')
cat('\nThere are ',sum(!(is.na(dt[,3]))),prm.os$EDRKeyword,' data points and ',
    sum(is.na(dt[,3])),' missing.')
cat('\nThere are ',sum(!(is.na(dt[,2])) & !(is.na(dt[,3]))),' overlapping historian and EDR time intervals.')
dt$timespanAfter <- 0
dt$timespanAfter[1:(nrow(dt)-1)] <- as.numeric(difftime(dt$time[2:nrow(dt)],
                                                        dt$time[1:(nrow(dt)-1)],units='secs'))
cat('\nFound ',sum(dt$timespanAfter>medianTimespanH),' timespan intervals greater than ',
          medianTimespanH,' seconds out of ', nrow(dt),' total.')
newtimes <- NULL
library(lubridate)
for (i in which(dt$timespanAfter >= (2*medianTimespanH))) {
  # Use force_tz() to avoid problems when converting back to POSIXct
  # timeValue <- as.numeric(force_tz(dt$time[i],tzone=""))
  # Removed the force_tz() because of error in R3.4.2
  # Now seems to work OK on both R3.2.2 and R3.4.3, FHS, Nov 27, 2017
  timeValue <- as.numeric(dt$time[i])
  # Faster code implemented, May 14, 2016 FHS
  j <- seq(timeValue+medianTimespanH,timeValue+dt$timespanAfter[i]-1,by=medianTimespanH)
  if (length(j) < 3*(abs(prm.os$offsetMin)+abs(prm.os$offsetMax))) {
    # small gap, fill in completely
    newtimes <- append(newtimes, as.POSIXct(j,origin="1970-01-01 00:00:00"))
  } else {
    # big gap, only fill in from edges with enough blanks to cover offsets
    newtimes <- append(newtimes, as.POSIXct(j[1:(abs(prm.os$offsetMin)+abs(prm.os$offsetMax))],
                                            origin="1970-01-01 00:00:00"))
    newtimes <- append(newtimes, as.POSIXct(j[(length(j)-abs(prm.os$offsetMin)-abs(prm.os$offsetMax)):length(j)],
                                            origin="1970-01-01 00:00:00"))
  }
}
cat('\nInserting ',length(newtimes),medianTimespanH,
    ' second blank intervals to cover the gaps for offset calculations.')
newtimes <- data.frame(time=newtimes)
if (nrow(newtimes)>0) {
  dt <- merge(dt,newtimes,all.x=T,all.y=T)
}
dt <- dt[order(dt$time),]

# Look at 'block height' correlations for range of time offset correlations
offset <- data.frame(offset=prm.os$offsetMin:prm.os$offsetMax)
offset$sec <- offset$offset*medianTimespanH
offset$corr <- rep(NA,nrow(offset))
offset$hist <- rep(NA,nrow(offset))
offset$EDR <- rep(NA,nrow(offset))

# First pass, evaluate fraction of offsets to get in vicinity of maximum
pass1StepSize <- 10 # step size

for (i in seq(from=1,to=nrow(offset),by=pass1StepSize)) {
  o <- offset$offset[i]
  if (o < 0) {
    h1 <- dt[1:(nrow(dt)+o),2]
    h2 <- dt[(-o+1):nrow(dt),2]
    e1 <- dt[1:(nrow(dt)+o),3]
    e2 <- dt[(-o+1):nrow(dt),3]
    offset$corr[i] <- cor(h2[complete.cases(h2,e1)],e1[complete.cases(h2,e1)])
    offset$EDR[i] <- cor(e1[complete.cases(e1,e2)],e2[complete.cases(e1,e2)])
    offset$hist[i] <- cor(h1[complete.cases(h1,h2)],h2[complete.cases(h1,h2)])
  } else {
    h1 <- dt[(o+1):nrow(dt),2]
    h2 <- dt[1:(nrow(dt)-o),2]
    e1 <- dt[(o+1):nrow(dt),3]
    e2 <- dt[1:(nrow(dt)-o),3]
    offset$corr[i] <- cor(h2[complete.cases(h2,e1)],e1[complete.cases(h2,e1)])
    offset$EDR[i] <- cor(e1[complete.cases(e1,e2)],e2[complete.cases(e1,e2)])
    offset$hist[i] <- cor(h1[complete.cases(h1,h2)],h2[complete.cases(h1,h2)])
  }
}

# If there are no correlation values between EDR and Historian data
# Shows error message and provides graceful exit

if (sum(!is.na(offset$corr))==0) {
  cat('\n\noffset ERROR could not compute any EDR/Historian correlations...likely caused by stdev=0 and too few matching observations...')
} else {
  pass1MaxIndex <- which(offset$corr==max(offset$corr,na.rm=T))[1]
  i1 <- max(1,pass1MaxIndex-2*pass1StepSize)
  i2 <- min(nrow(offset),pass1MaxIndex+2*pass1StepSize)
  pass1MaxIndex <- which(offset$sec==0)[1]
  i3 <- max(1,pass1MaxIndex-2*pass1StepSize)
  i4 <- min(nrow(offset),pass1MaxIndex+2*pass1StepSize)

  for (i in c(i1:i2,i3:i4)) {
    o <- offset$offset[i]
    if (o < 0) {
      h1 <- dt[1:(nrow(dt)+o),2]
      h2 <- dt[(-o+1):nrow(dt),2]
      e1 <- dt[1:(nrow(dt)+o),3]
      e2 <- dt[(-o+1):nrow(dt),3]
      offset$corr[i] <- cor(h2[complete.cases(h2,e1)],e1[complete.cases(h2,e1)])
      offset$EDR[i] <- cor(e1[complete.cases(e1,e2)],e2[complete.cases(e1,e2)])
      offset$hist[i] <- cor(h1[complete.cases(h1,h2)],h2[complete.cases(h1,h2)])
    } else {
      h1 <- dt[(o+1):nrow(dt),2]
      h2 <- dt[1:(nrow(dt)-o),2]
      e1 <- dt[(o+1):nrow(dt),3]
      e2 <- dt[1:(nrow(dt)-o),3]
      offset$corr[i] <- cor(h2[complete.cases(h2,e1)],e1[complete.cases(h2,e1)])
      offset$EDR[i] <- cor(e1[complete.cases(e1,e2)],e2[complete.cases(e1,e2)])
      offset$hist[i] <- cor(h1[complete.cases(h1,h2)],h2[complete.cases(h1,h2)])
    }
  }
  
  cat('\n\nMax Historian ',prm.os$histKeyword,' to EDR ',prm.os$EDRKeyword,
  ' correlation=',round(max(offset$corr,na.rm=T),digits=3))
  cat('\nHistorian Time Offset: seconds to be added to Historian time for max correlation=',
      offset$sec[which(offset$corr==max(offset$corr,na.rm=T))],'\n\n')
  
  if(!(prm.os$plotName=='')) png(filename=prm.os$plotName)
  plot(offset$sec,offset$corr, type='p',pch=20,cex=0.5,col='black',
       ylim=c((floor(min(offset[,3:5],na.rm=TRUE)*medianTimespanH)/medianTimespanH),1),
       main=sprintf('%s %s Time Offset Correlations',prm.os$EDRKeyword,prm.os$histKeyword),
       xlab='Time offset Historian minus EDR (seconds)',ylab='Correlation',
       sub=sprintf('Maximize corr=%.3f by adding %.0f seconds to Historian timestamp',
                   max(offset$corr,na.rm=T),offset$sec[which(offset$corr==max(offset$corr,na.rm=T))]))
  lines(offset$sec,offset$corr,col='black')
  lines(offset$sec,offset$hist,col='red')
  lines(offset$sec,offset$EDR,col='green')
  points(offset$sec,offset$hist,col='red',pch=20,cex=0.5)
  points(offset$sec,offset$EDR,col='green',pch=20,cex=0.5)
  legend('topleft',c('Hist-Hist','EDR-EDR','Hist-EDR'),fill=c('red','green','black'))
  if(!(prm.os$plotName=='')) dev.off()

}

stop.time <- Sys.time()
cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')
cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
