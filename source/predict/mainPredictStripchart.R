#######################################################################################
# mainPredictStripchart.R V3.0 - main routine for producing prediction strip chart
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 10, 2017
#######################################################################################

sourceDir <- "E:/Analytics/Rcode/190401_master/"
source(paste0(sourceDir,"source/predict/get.prm.predictStripchart.R"))
source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/predict/predictStripchart.R"))
source(paste0(sourceDir,"source/predict/rfcutPlot.R"))

start.time <- Sys.time()

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/predict/predictStripchart.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Get the predict stripchart parameters
cat('\nLoading predict strip chart parameters from file ',prmFilename,'\n\n')
prm.ps <- get.prm.predictStripchart(prmFilename)

cat(prm.ps$version,'\nRun Started at : ',as.character(start.time),'\n')

cat('\nVerbatim listing of prediction parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.ps$text)) { cat(prm.ps$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

# Load the file from which prediction plots are to be made
cat('\nReading predictions data file: \n',prm.ps$predictFilename)
dt <- read.csv(prm.ps$predictFilename,nrow=prm.ps$to)
cat('\nRead ',nrow(dt),' rows and ',ncol(dt),' columns.')

# If futureIgbtMax not present, but igbt_temp is, then creates futureIgbtMax
if (is.null(dt$futureIgbtMax) & !is.null(dt$igbt_temp)) {
  cat('\ncreating futureIgbtMax')
  library(caTools)
  dt$futureIgbtMax <- NA
  dt$futureIgbtMax[1:(nrow(dt)-1)] <-
    runmax(dt$igbt_temp[2:nrow(dt)],k=(prm.ps$rowFutureSpan-1),align='left')
  dt$futureIgbtMax[dt$futureIgbtMax==-Inf] <- NA
}

requiredColumns <- c('time','block_height','igbt_temp','hookload',
                     'futureIgbtMax','rfGTcut','predicted','rfAlarm')

if (length(requiredColumns)>sum(requiredColumns %in% colnames(dt))) {
  cat('\npredictions data file missing columns:\n')
  cat(requiredColumns[which(!requiredColumns %in% colnames(dt))])
  cat('\nunable to continue...')
  stop('Missing Columns for predict plots in file')
}

#########################################################################
# Input data generated, now produce plots

if(!(prm.ps$plotFilename=='')) pdf(file=prm.ps$plotFilename)

# First plot is rfcutoff chart
# Only consider observations where targetSource value at present time is below alarm cutoff
predict_select <- dt$igbt_temp < prm.ps$targetAlarmCutoff & 
  !is.na(dt$rfGTcut) & !is.na(dt$futureIgbtMax) & !is.na(dt$igbt_temp)
rfGTcutSelect <- dt$rfGTcut[predict_select]
cat('\nlength(rfGTcutSelect)=',length(rfGTcutSelect))
# indicator vectors for prediction target below and above alarm cutoff
targetBelowCutoff <- dt$futureIgbtMax[predict_select]<prm.ps$targetAlarmCutoff
targetAboveCutoff <- dt$futureIgbtMax[predict_select]>=prm.ps$targetAlarmCutoff

if (sum(targetBelowCutoff,na.rm=T)==0 | sum(targetAboveCutoff,na.rm=T)==0) {
  cat('\n\nCount of futureIgbtMax values below cutoff=',sum(targetBelowCutoff,na.rm=T))
  cat('\nCount of futureIgbtMax values above cutoff=',sum(targetAboveCutoff,na.rm=T))
  cat('\nThere must be futureIgbtMax values both above and below cutoff to proceed with cutoff analysis.\n')

} else {
  # There are some values above cutoff and some values below cutoff
  # can proceed with analysis
  rfcutPredict <- data.frame(cutoff=seq(0,1.00,.01))
  rfcutPredict$trueNegative <- apply(as.matrix(rfcutPredict$cutoff,ncol=1),MARGIN=1,function(x) {
    return(sum(targetBelowCutoff & rfGTcutSelect<=x))})
  rfcutPredict$falseNegative <- apply(as.matrix(rfcutPredict$cutoff,ncol=1),MARGIN=1,function(x) {
    return(sum(targetAboveCutoff & rfGTcutSelect<=x))})
  rfcutPredict$falsePositive <- apply(as.matrix(rfcutPredict$cutoff,ncol=1),MARGIN=1,function(x) {
    return(sum(targetBelowCutoff & rfGTcutSelect>x))})
  rfcutPredict$truePositive <- apply(as.matrix(rfcutPredict$cutoff,ncol=1),MARGIN=1,function(x) {
    return(sum(targetAboveCutoff & rfGTcutSelect>x))})
  
  rfcutPredict$sensitivity <- rfcutPredict$truePositive/(rfcutPredict$truePositive+rfcutPredict$falseNegative)
  rfcutPredict$sensitivity[is.na(rfcutPredict$sensitivity)] <- 0
  rfcutPredict$specificity <- rfcutPredict$trueNegative/(rfcutPredict$trueNegative+rfcutPredict$falsePositive)
  rfcutPredict$specificity [is.na(rfcutPredict$specificity)] <- 0
  rfcutPredict$posPredValue <- rfcutPredict$truePositive/(rfcutPredict$truePositive+rfcutPredict$falsePositive)
  rfcutPredict$posPredValue[is.na(rfcutPredict$posPredValue)] <- 0
  rfcutPredict$negPredValue <- rfcutPredict$trueNegative/(rfcutPredict$trueNegative+rfcutPredict$falseNegative)
  rfcutPredict$negPredValue[is.na(rfcutPredict$negPredValue)] <- 0
  rfcutPredict$f_score <- (1+prm.ps$ppvFscoreFactor)*(rfcutPredict$posPredValue*rfcutPredict$sensitivity)/
    (prm.ps$ppvFscoreFactor*rfcutPredict$posPredValue+rfcutPredict$sensitivity)
  rfcutPredict$f_score[is.na(rfcutPredict$f_score)] <- 0
  # 
  cat('\nPrediction Data Optimum f_score rf tree threshold cutoff\n')
  print(rfcutPredict[rfcutPredict$f_score==max(rfcutPredict$f_score),])
  # prm.ps$maxF_scoreCutoff <- rfcutPredict$cutoff[rfcutPredict$f_score==max(rfcutPredict$f_score)][1]
  # prm.ps$maxF_scoreCutoff <- prm.ps$rfGTCutoffAlarm
  
  rfcutplot(rfcutPredict,sprintf('RF Vote Cutoff %i minute future %s',
                                 round(prm.ps$rowFutureSpan/6),'igbt_temp'),prm.ps)
}

# second plot, scatterplot of actual futureIgbtMax vs rfGTcutAlarm
# True negatives
selectedTN <- dt$futureIgbtMax<prm.ps$targetAlarmCutoff & !dt$rfAlarm
plot(dt$futureIgbtMax[selectedTN],dt$rfGTcut[selectedTN], pch='.',col='green',
     main=sprintf('Actual futureIgbtMax vs rfGTcut alarm'),
     xlab='actual futureIgbtMax',ylab='rfGTcut alarm',
     xlim=c(min(dt$predicted,dt$futureIgbtMax,na.rm=T),max(dt$predicted,dt$futureIgbtMax,na.rm=T)),
     ylim=c(-0.05, 1.05))
# False negatives
selectedFN <- dt$futureIgbtMax>=prm.ps$targetAlarmCutoff & !dt$rfAlarm
points(dt$futureIgbtMax[selectedFN],dt$rfGTcut[selectedFN], pch='.',col='black')
# True positives
selectedTP <- dt$futureIgbtMax>=prm.ps$targetAlarmCutoff & dt$rfAlarm
points(dt$futureIgbtMax[selectedTP],dt$rfGTcut[selectedTP], pch='.',col='red')
# False positives
selectedFP <- dt$futureIgbtMax<prm.ps$targetAlarmCutoff & dt$rfAlarm
points(dt$futureIgbtMax[selectedFP],dt$rfGTcut[selectedFP], pch='.',col='blue')
obsCount <- sum(selectedTN,na.rm=T)+sum(selectedFN,na.rm=T)+sum(selectedTP,na.rm=T)+sum(selectedFP,na.rm=T)

legend('bottomleft',c(sprintf('False Positive (%.1f%%)',round(100*sum(selectedFP,na.rm=T)/obsCount,digits=1)),
                      sprintf('True Positive  (%.1f%%)',round(100*sum(selectedTP,na.rm=T)/obsCount,digits=1)),
                      sprintf('True Negative  (%.1f%%)',round(100*sum(selectedTN,na.rm=T)/obsCount,digits=1)),
                      sprintf('False Negative (%.1f%%)',round(100*sum(selectedFN,na.rm=T)/obsCount,digits=1))),
       fill=c('blue','red','green','black'),cex=0.8)

# Third plot is the high alarm event count
# Analysis of prediction performance
dt$presentTarget <- 'lowp'
dt$presentTarget[dt$igbt_temp>=prm.ps$targetAlarmCutoff & !is.na(dt$igbt_temp)] <- 'highp'
dt$presentTarget <- as.factor(dt$presentTarget)

targetrun <- data.frame(values=rle(as.vector(dt$presentTarget))$values,
                        lengths=rle(as.vector(dt$presentTarget))$lengths)
targetrun$from <- 0
targetrun$to <- 0
targetrun$to <- cumsum(targetrun$lengths)
targetrun$from <- targetrun$to - targetrun$lengths + 1
# Agglomerate highp runs that are separated by less than rowFutureSpan lowp values
dt$presentTargetAgglomerated <- dt$presentTarget
for (i in which(targetrun$values=='lowp' & targetrun$lengths<(prm.ps$rowFutureSpan))) {
  dt$presentTargetAgglomerated[targetrun$from[i]:targetrun$to[i]] <- 'highp'
}
dt$presentTargetAgglomerated <- as.factor(dt$presentTargetAgglomerated)
targetrunA <- data.frame(values=rle(as.vector(dt$presentTargetAgglomerated))$values,
                         lengths=rle(as.vector(dt$presentTargetAgglomerated))$lengths)
targetrunA$from <- 0
targetrunA$to <- 0
targetrunA$to <- cumsum(targetrunA$lengths)
targetrunA$from <- targetrunA$to - targetrunA$lengths + 1
cat('\nCount of igbt_temp observations below and above alarm cutoff')
print(table(dt$presentTarget)[c('lowp','highp')])
cat('\nCount of igbt_temp observation runs below and above alarm cutoff')
print(table(targetrun$values)[c('lowp','highp')])
cat('\nCount of igbt_temp agglomerated observation runs below and above alarm cutoff')
print(table(targetrunA$values)[c('lowp','highp')])

# Gather some highp statistics
targetrunA$highp <- 0
targetrunA$highpDensity <- 0
targetrunA$AdvancedAlarmTime <- 0
targetrunA$AlarmDensity <- 0
for (i in which(targetrunA$values=='highp')) {
  # Number of 'highp' (present value GT cutoff) records in current 'highp' interval 
  targetrunA$highp[i] <- sum(dt$presentTarget[targetrunA$from[i]:targetrunA$to[i]]=='highp')
  
  i1 <- targetrunA$from[i] - prm.ps$rowFutureSpan
  i2 <- targetrunA$from[i] - 1
  if (i1>0) {
    i3 <- if (length(which(dt$rfAlarm[i1:i2]==TRUE))>0) i1 + min(which(dt$rfAlarm[i1:i2]==TRUE)) - 1 else -1
  } else {
    i3 <- -1
  }
  if (i3>0) {
    targetrunA$AdvancedAlarmTime[i] <- round((i2 - i3 + 1)/6,digits=1)
    targetrunA$AlarmDensity[i] <- round(length(which(dt$rfAlarm[i3:i2]==TRUE))/(i2-i3+1),digits=3)
  } else {
    targetrunA$AdvancedAlarmTime[i] <- -5
  }
}
targetrunA$highpDensity <- round(targetrunA$highp/targetrunA$lengths,digits=3)

targetrunA$FalseAlarmDensity <- 0
for (i in which(targetrunA$values=='lowp')) {
  i1 <- targetrunA$from[i] + 2*prm.ps$rowFutureSpan 
  i2 <- targetrunA$to[i] - 2*prm.ps$rowFutureSpan
  if (i2>i1) targetrunA$FalseAlarmDensity[i] <- round(length(which(dt$rfAlarm[i1:i2]==TRUE))/(i2-i1+1),digits=3) else 0
}

if (length(targetrunA$AdvancedAlarmTime[targetrunA$values=='highp' & 
                                        targetrunA$highp>=prm.ps$highCountForHist])>0) {
  hist(targetrunA$AdvancedAlarmTime[targetrunA$values=='highp' & targetrunA$highp>=prm.ps$highCountForHist],
       breaks=(5+prm.ps$rowFutureSpan/6),
       xlab='Alarm Time in Advance of High Alarm Event (minutes)',
       ylab='High Alarm Event Count',
       main=sprintf('High %s Alarm Events from %s to %s','igbt_temp',substr(dt$time[1],1,10),substr(dt$time[nrow(dt)],1,10)))
}









# Fourth plot and beyond strip chart plots of selected prediction data
if (prm.ps$interval>0) {
  for (from in seq(prm.ps$from,nrow(dt),prm.ps$interval)) {
    to <- from + prm.ps$interval - 1
    predictStripchart(dt,prm.ps$title, prm.ps$targetAlarmCutoff, from, to)
  }
}

if(!(prm.ps$plotFilename=='')) dev.off()
