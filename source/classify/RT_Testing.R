# Testing of RTclassify versus classify data
# FHS May 26, 2017

dir <- 'C:/Users/Fred.Seymour/EDR_Data/170519_Testing/170527_rig140_RTclassify/'

fileRT <- 'rig140_diagnostics_170527141203.csv'
fileBAT <- 'input_EDR_file_clean_diagnostic_diagnostic.csv'

dtRT <- read.csv(paste0(dir,fileRT))
cat('\nLoaded ',fileRT,' with ',nrow(dtRT),' rows and ',ncol(dtRT),' columns.')
dtRT$time <- as.character(dtRT$time)

dtBAT <- read.csv(paste0(dir,fileBAT))
cat('\nLoaded ',fileBAT,' with ',nrow(dtBAT),' rows and ',ncol(dtBAT),' columns.')
dtBAT$time <- as.character(dtBAT$time)

# Fix for selected columns
# dtRT$EDR_RotaryTorque <- dtRT$EDR_RotaryTorque * 0.00025
# dtRT$EDR_StandpipePressure <- (dtRT$EDR_StandpipePressure - 4250)  * 4.4
# dtRT$EDR_StandpipePressure <- dtRT$EDR_StandpipePressure  * 19.1
# dtRT$EDR_HookLoad <- dtRT$EDR_HookLoad * 2.25
# dtRT$EDR_WOB <- dtRT$EDR_WOB * 3.0
# 
# file.writeRT <- 'rig140_diagnosticsGtest.csv'
# write.csv(dtRT,file=paste0(dir,file.writeRT),row.names=FALSE)


dtBAT2 <- dtBAT[dtBAT$time %in% dtRT$time,]
# dtBAT2$EDR_RotaryTorque <- dtBAT2$EDR_RotaryTorque * 737.56

dtRT$time <- as.POSIXlt(dtRT$time,"%Y-%m-%d %H:%M:%S",tz="")
dtBAT2$time <- as.POSIXlt(dtBAT2$time,"%Y-%m-%d %H:%M:%S",tz="")

# Detailed view of subset configured here
f <- 1
t <- nrow(dtRT)

if (nrow(dtBAT2)==nrow(dtRT)) {
  dtRT <- dtRT[f:t,]
  dtBAT2 <- dtBAT2[f:t,]
}

# # # Selectively transfer batch predictors to real time dataset
# fileOUT <- 'rig140_diagnosticsQ.csv'
# dtOUT <- dtRT
# # dtOUT$EDR_HoleDepth <- dtBAT2$EDR_HoleDepth
# dtOUT$EDR_BitDepth <- dtBAT2$EDR_BitDepth
# dtOUT$EDR_BlockHeight <- dtBAT2$EDR_BlockHeight
# dtOUT$EDR_HookLoad <- dtBAT2$EDR_HookLoad
# dtOUT$EDR_ROP <- dtBAT2$EDR_ROP
# dtOUT$EDR_WOB <- dtBAT2$EDR_WOB
# dtOUT$EDR_RotaryRPM <- dtBAT2$EDR_RotaryRPM
# dtOUT$EDR_StandpipePressure <- dtBAT2$EDR_StandpipePressure
# dtOUT$EDR_RotaryTorque <- dtBAT2$EDR_RotaryTorque
# write.csv(dtOUT,file=paste0(dir,fileOUT),row.names=FALSE)

# Produce comparison charts
# Lists UOMs in chart legend where applicable
predictorList <- c('EDR_BlockHeight','BlockHeight_UOM',
                   'EDR_HoleDepth','HoleDepth_UOM',
                   'EDR_BitDepth','BitDepth_UOM',
                   'EDR_HookLoad','Hookload_UOM',
                   'EDR_StandpipePressure','PumpPressure_UOM',
                   'EDR_RotaryRPM','RPM_UOM',
                   'EDR_WOB','WeightOnBit_UOM',
                   'EDR_ROP','OnBottomROP_UOM',
                   'EDR_RotaryTorque','Torque_UOM',
                   'bitHoleDepthRatio','NA',
                   'holeDepthSmooth','HoleDepth_UOM',
                   'deltaHoleDepthSmooth','HoleDepth_UOM',
                   'bitDepthSmooth','BitDepth_UOM',
                   'deltaBitDepthSmooth','BitDepth_UOM',
                   'bitSpeed','NA',
                   'bitDistFromHoleBottom','NA',
                   'deltaBitDepth','BitDepth_UOM',
                   'deltaHoleDepth','HoleDepth_UOM',
                   'deltaBlockHeightAbs','BlockHeight_UOM',
                   'blockHeightRunLengths','NA',
                   'blockHeightRunMean','BlockHeight_UOM',
                   'blockHeightRunSD','BlockHeight_UOM',
                   'hookLoadRunMean','Hookload_UOM',
                   'hookLoadRunSD','Hookload_UOM',
                   'standpipePressureRunMean','PumpPressure_UOM',
                   'standpipePressureRunSD','PumpPressure_UOM',
                   'rotaryRPMRunMean','RPM_UOM',
                   'rotaryRPMRunSD','RPM_UOM')

predictorList <- t(matrix(predictorList,nrow=2))
predictorList <- as.data.frame(predictorList)
colnames(predictorList) <- c('predictor','UOM')


# Look at matching rig_states
dtRT$rig_state <- as.character(dtRT$rig_state)
dtBAT2$rig_state <- as.character(dtBAT2$rig_state)

selectRT <- dtRT$time %in% dtBAT2$time
selectBAT2 <- dtBAT2$time %in% dtRT$time

rig_state_match <- data.frame(time=dtRT$time[selectRT],
                              rig_state_RT=dtRT$rig_state[selectRT],
                              rig_state_BAT=dtBAT2$rig_state[selectBAT2],
                              match=dtRT$rig_state[selectRT]==dtBAT2$rig_state[selectBAT2])

# plot(rig_state_match$time,rig_state_match$match,type='l',col='green',
#      main='Rig State Match')

for (i in 1:nrow(predictorList)) {
  p <- as.character(predictorList$predictor[i])
  if (!is.null(dtRT[[p]]) & !is.null(dtBAT2[[p]])) {
    cat('\nPlotting predictor ',p,' mis-match count=',sum(dtRT[[p]] != dtBAT2[[p]],na.rm=T),
        ' NA mis-match count=',sum(which(is.na(dtRT[[p]])) != which(is.na(dtBAT2[[p]]))))
    miny <- min(dtRT[[p]],dtBAT2[[p]],na.rm=T)
    maxy <- max(dtRT[[p]],dtBAT2[[p]],na.rm=T)
    plot(dtRT$time,dtRT[[p]],type='l',col='red',main=p,
         ylim=c(miny, maxy))
    lines(dtBAT2$time,dtBAT2[[p]],col='blue')
    legend('topleft',c('Batch','RealTime'),fill=c('Blue','Red'))
    # Indicate rig state mismatches
    rig_state_match$y <- (miny+maxy)/2
    points(rig_state_match$time[!rig_state_match$match],
           rig_state_match$y[!rig_state_match$match],pch=20,col='black')
    batchString <- if (is.null(dtBAT2[[as.character(predictorList$UOM[i])]])) 'Batch' else
      paste0('Batch ',dtBAT2[[as.character(predictorList$UOM[i])]][1])
    realtimeString <- if (is.null(dtRT[[as.character(predictorList$UOM[i])]])) 'RealTime' else
      paste0('RealTime ',dtRT[[as.character(predictorList$UOM[i])]][1])
    legend('topleft',c(batchString,realtimeString,'mismatch'),fill=c('Blue','Red','black'))
  } else {
    cat('\nCannot plot predictor',p,' because\n     ',
        paste0('is.null(dtRT$',p,')='),is.null(dtRT[[p]]),'\n     ',
        paste0('is.null(dtBAT2$',p,')='),is.null(dtBAT2[[p]]))
  }
  
  # plot(dtRT[[p]][selectRT],dtBAT2[[p]][selectBAT2],pch=20,col='green',main=p)
}

cat('\n\nTotal Number of Matching Rig States=',sum(rig_state_match$match),
    ' out of ',nrow(dtRT))
cat('\nTotal Number of Matching Rig State rfvotes=',sum(dtRT$rfvote==dtBAT2$rfvote),
    ' out of ',nrow(dtRT))

cat('\n\nTable of rig_state matches (row=RT, col=BAT)')
print(table(dtRT$rig_state,dtBAT2$rig_state))

