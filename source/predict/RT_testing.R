# Testing of RTpredict versus historian data
# FHS May 12, 2017

dir <- 'C:/Users/Fred.Seymour/Historian_Data/170407_IGBT_temp_predict/170512_rig140/'

fileRT <- 'rig140_RTdiagnostics.csv'
fileBAT <- 'rig140_historian_clean_diagnostic.csv'
# file.pdf <- 'rig140_comparison.pdf'
file.pdf <- ''

dtRT <- read.csv(paste0(dir,fileRT))
cat('\nLoaded ',fileRT,' with ',nrow(dtRT),' rows and ',ncol(dtRT),' columns.')
dtRT$time <- as.character(dtRT$time)

dtBAT <- read.csv(paste0(dir,fileBAT))
cat('\nLoaded ',fileBAT,' with ',nrow(dtBAT),' rows and ',ncol(dtBAT),' columns.')
dtBAT$time <- as.character(dtBAT$time)

dtBAT2 <- dtBAT[dtBAT$time %in% dtRT$time,]
# dtBAT2$hookload <- dtBAT2$hookload*3.897
# dtBAT2$hookloadRunMean <- dtBAT2$hookloadRunMean*3.897
# dtBAT2$hookloadRunSD <- dtBAT2$hookloadRunSD*3.897

dtRT$time <- as.POSIXlt(dtRT$time,"%Y-%m-%d %H:%M:%S",tz="")
dtBAT2$time <- as.POSIXlt(dtBAT2$time,"%Y-%m-%d %H:%M:%S",tz="")

# Produce comparison charts
predictorList <- c('block_height',
                   'deltaBlockHeightAbs',
                   'dc_bus_voltage',
                   'dc_bus_voltageRunMean',
                   'dc_bus_voltageRunSD',
                   'output_voltage',
                   'output_voltageRunMean',
                   'output_voltageRunSD',
                   'current',
                   'currentRunMean',
                   'currentRunSD',
                   'torque',
                   'torqueRunMean',
                   'torqueRunSD',
                   'speed',
                   'speedRunMean',
                   'speedRunSD',
                   'frequency',
                   'frequencyRunMean',
                   'frequencyRunSD',
                   'hookload',
                   'hookloadRunMean',
                   'hookloadRunSD',
                   'igbt_temp',
                   'igbt_tempRunMean',
                   'igbt_tempRunSD',
                   'predicted',
                   'rfGTcut')

if(!(file.pdf=='')) pdf(file=paste0(dir,file.pdf))

for (p in predictorList) {
  cat('\nPlotting predictor ',p)
  miny <- min(dtRT[[p]],dtBAT2[[p]],na.rm=T)
  maxy <- max(dtRT[[p]],dtBAT2[[p]],na.rm=T)
  plot(dtRT$time,dtRT[[p]],type='l',col='red',main=p,
       ylim=c(miny, maxy))
  lines(dtBAT2$time,dtBAT2[[p]],col='blue')
  legend('topleft',c('Historian Batch','Rig140 RealTime'),fill=c('Blue','Red'))
}

if(!(file.pdf=='')) dev.off()

# Look at real time results file
file.results <- 'rig140_results.csv'
dt.results <- read.csv(paste0(dir,file.results))
cat('\nLoaded ',file.results,' with ',nrow(dt.results),' rows and ',ncol(dt.results),' columns.')
dt.results$time <- as.POSIXlt(dt.results$time,"%Y-%m-%d %H:%M:%S",tz="")
miny <- 0
maxy <- max(dt.results$errorCount,dt.results$mismatch,na.rm=T)
plot(dt.results$time,dt.results$errorCount,type='l',col='red',main='Rig140 Real Time SQL Anomalies',
     ylim=c(miny, maxy))
lines(dt.results$time,dt.results$mismatch,col='cyan')
legend('topleft',c('Data Mismatch Count','SQL Query Error Count'),fill=c('Cyan','Red'))

