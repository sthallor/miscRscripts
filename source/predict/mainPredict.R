#######################################################################################
# mainPredict.R V3.0 - main routine for numeric prediction from random forest model
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Apr 28, 2017
#######################################################################################

# clear the memory
# rm(list=ls())

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code master directory absolute address
library(randomForest)
library(caTools)
sourceDir <- "E:/Analytics/Rcode/190401_master/"
source(paste0(sourceDir,"source/predict/get.prm.predictTrain.R"))
source(paste0(sourceDir,"source/predict/get.prm.predict.R"))
source(paste0(sourceDir,"source/predict/dbuildPredict.R"))
source(paste0(sourceDir,"source/predict/rfcutplot.R"))
source(paste0(sourceDir,"source/predict/predictStripchart.R"))
source(paste0(sourceDir,"source/util/prm.decode.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/predict/predict.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Get the training parameters
cat('\nLoading prediction parameters from file ',prmFilename,'\n\n')
prm.p <- get.prm.predict(prmFilename)

cat('\nLoading previous training parameters from file ',prm.p$trainPrmFilename,'\n')
prm.pt <- get.prm.predictTrain(prm.p$trainPrmFilename)

if (nchar(prm.p$outputFilename)>0) {
  cat('Predictions from Rig State Model results sending output to:\n',prm.p$outputFilename,'\n')
  sink(file=prm.p$outputFilename)
  options(width=132)
}

cat(prm.p$version,'\nRun Started at : ',as.character(start.time),'\n')

cat('\nVerbatim listing of prediction parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.p$text)) { cat(prm.p$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

cat('\nVerbatim listing of training parameters file from:\n',prm.p$trainPrmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.pt$text)) { cat(prm.pt$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

cat('\nLoading the Random Forest Classification Model (binary file) : \n',prm.pt$rfFilename)
load(file=prm.pt$rfFilename)
cat('\nThe memory consumed by the model is ');print(object.size(rf),units='Mb')
cat('\nThe model had a total of ',length(rf$y),' training observations.')
cat('\n',length(which(rf$y>=prm.pt$targetAlarmCutoff)),
    ' observations above ',prm.pt$targetSource,
    ' cutoff =',prm.pt$targetAlarmCutoff,' and ',
    length(which(rf$y<prm.pt$targetAlarmCutoff)),' below cutoff')
cat('\nntree=',prm.pt$ntree,' mtry=',prm.pt$mtry)

# Load the file on which to perform the predictions
cat('\nReading data file for predictions: \n',prm.p$predictFilename)
dt <- read.csv(prm.p$predictFilename,nrow=prm.p$nrowPredict)
cat('\nRead ',nrow(dt),' rows and ',ncol(dt),' columns.')

# Only keep the predictors needed for building and training
dt <- dt[,colnames(dt) %in% c('time','Rig',prm.pt$cpnames,prm.pt$target,
                              unique(prm.pt$standardUOM$uomColName))]
cat('\n\nRetaining ',ncol(dt),' input file columns for predictions with random forest model.\n')

dt <- tryCatch({
  dbuildPredict(dt,prm.pt)
},
error=function(e) {
  cat('\n\nERRORS in dbuildPredict, UNABLE TO PERFORM HISTORIAN PREDICTION:\n',e$message,'\n\n')
  errorFlag <- TRUE
  dt <- NULL
})

dt <- dt[,c('time',prm.pt$cpnames,prm.pt$tpnames,prm.pt$target)]
dt$time <- as.character(dt$time) # Needed for complete.cases to work... FHS Mar 8, 2017
dt <- dt[complete.cases(dt),]
cat('\nThere are a total of ',length(which(dt[[prm.pt$target]]>=prm.pt$targetAlarmCutoff)),prm.pt$target,
    ' observations above cutoff=',prm.pt$targetAlarmCutoff,
    ' and ',length(which(dt[[prm.pt$target]]<prm.pt$targetAlarmCutoff)),' observations below cutoff.\n')

cat('\nProducing random forest prediction on dataset with ',nrow(dt),' total observations.\n')

dt$rfGTcut <- 0
dt$predicted <- 0
for (chunk in seq(1,nrow(dt),prm.p$predictChunkSize)) {
  c1 <- chunk
  c2 <- min((chunk+prm.p$predictChunkSize-1),nrow(dt))
  dt1 <- dt[c1:c2,]
  rf.all <- predict(rf,dt1,predict.all=TRUE)
  # cat('\nThe memory consumed by individual tree random forest predictions is ');print(object.size(rf.all),units='Mb')
  dt$predicted[c1:c2] <- rf.all$aggregate
  
  # Compute rfcutPredict table and optimal random forest tree prediction proportion above alarm threshold cutoff
  # For each observation, determine proportion of random forest tree predictions that are greater than alarm cutoff
  dt$rfGTcut[c1:c2] <- apply(rf.all$individual,MARGIN=1,function(x) {sum(x>=prm.pt$targetAlarmCutoff)/length(x)})
  cat('\nPredicted chunk from observation ',c1,' to ',c2,' with mean proportion=',mean(dt$rfGTcut[c1:c2],na.rm=T),
      ' above alarm cutoff=',prm.pt$targetAlarmCutoff)
}

# Only consider observations where targetSource value at present time is below alarm cutoff
predict_select <- dt[[prm.pt$targetSource]] < prm.pt$targetAlarmCutoff 
rfGTcutSelect <- dt$rfGTcut[predict_select]
# indicator vectors for prediction target below and above alarm cutoff
targetBelowCutoff <- dt[[prm.pt$target]][predict_select]<prm.pt$targetAlarmCutoff
targetAboveCutoff <- dt[[prm.pt$target]][predict_select]>=prm.pt$targetAlarmCutoff

if(!(prm.p$plotFilename=='')) pdf(file=prm.p$plotFilename)

if (sum(targetBelowCutoff,na.rm=T)==0 | sum(targetAboveCutoff,na.rm=T)==0) {
  cat('\n\nCount of ',prm.pt$target,' values below cutoff=',sum(targetBelowCutoff,na.rm=T))
  cat('\nCount of ',prm.pt$target,' values above cutoff=',sum(targetAboveCutoff,na.rm=T))
  cat('\nThere must be ',prm.pt$target,' values both above and below cutoff to proceed with cutoff analysis.\n')
  if (sum(targetAboveCutoff,na.rm=T)==0) dt$rfAlarm <- FALSE else dt$rfAlarm <- TRUE
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
  rfcutPredict$f_score <- (1+prm.p$ppvFscoreFactor)*(rfcutPredict$posPredValue*rfcutPredict$sensitivity)/
    (prm.p$ppvFscoreFactor*rfcutPredict$posPredValue+rfcutPredict$sensitivity)
  rfcutPredict$f_score[is.na(rfcutPredict$f_score)] <- 0
  # 
  cat('\nPrediction Data Optimum f_score rf tree threshold cutoff\n')
  print(rfcutPredict[rfcutPredict$f_score==max(rfcutPredict$f_score),])
  prm.p$maxF_scoreCutoff <- rfcutPredict$cutoff[rfcutPredict$f_score==max(rfcutPredict$f_score)][1]
  
  rfcutplot(rfcutPredict,sprintf('RF Vote Cutoff %i minute future %s',
                                 round(prm.pt$rowFutureSpan/6),prm.pt$targetSource),prm.p)
  
  # Analysis of prediction performance
  dt$presentTarget <- 'lowp'
  dt$presentTarget[dt[[prm.pt$targetSource]]>=prm.pt$targetAlarmCutoff] <- 'highp'
  dt$presentTarget <- as.factor(dt$presentTarget)
  
  optimalCutoff <- rfcutPredict$cutoff[rfcutPredict$f_score==max(rfcutPredict$f_score)][1]
  
  dt$rfAlarm <- FALSE
  dt$rfAlarm[dt$rfGTcut>=optimalCutoff] <- TRUE
  
  targetrun <- data.frame(values=rle(as.vector(dt$presentTarget))$values,
                          lengths=rle(as.vector(dt$presentTarget))$lengths)
  targetrun$from <- 0
  targetrun$to <- 0
  targetrun$to <- cumsum(targetrun$lengths)
  targetrun$from <- targetrun$to - targetrun$lengths + 1
  # Agglomerate highp runs that are separated by less than rowFutureSpan lowp values
  dt$presentTargetAgglomerated <- dt$presentTarget
  for (i in which(targetrun$values=='lowp' & targetrun$lengths<(prm.pt$rowFutureSpan))) {
    dt$presentTargetAgglomerated[targetrun$from[i]:targetrun$to[i]] <- 'highp'
  }
  dt$presentTargetAgglomerated <- as.factor(dt$presentTargetAgglomerated)
  targetrunA <- data.frame(values=rle(as.vector(dt$presentTargetAgglomerated))$values,
                           lengths=rle(as.vector(dt$presentTargetAgglomerated))$lengths)
  targetrunA$from <- 0
  targetrunA$to <- 0
  targetrunA$to <- cumsum(targetrunA$lengths)
  targetrunA$from <- targetrunA$to - targetrunA$lengths + 1
  cat('\nCount of ',prm.pt$targetSource,' observations below and above alarm cutoff')
  print(table(dt$presentTarget)[c('lowp','highp')])
  cat('\nCount of ',prm.pt$targetSource,' observation runs below and above alarm cutoff')
  print(table(targetrun$values)[c('lowp','highp')])
  cat('\nCount of ',prm.pt$targetSource,' agglomerated observation runs below and above alarm cutoff')
  print(table(targetrunA$values)[c('lowp','highp')])

  # Gather some highp statistics
  targetrunA$highp <- 0
  targetrunA$highpDensity <- 0
  targetrunA$AdvancedAlarmTime <- 0
  targetrunA$AlarmDensity <- 0
  for (i in which(targetrunA$values=='highp')) {
    targetrunA$highp[i] <- sum(dt$presentTarget[targetrunA$from[i]:targetrunA$to[i]]=='highp')
    i1 <- targetrunA$from[i] - prm.pt$rowFutureSpan
    i2 <- targetrunA$from[i] - 1
    i3 <- if (length(which(dt$rfAlarm[i1:i2]==TRUE))>0) i1 + min(which(dt$rfAlarm[i1:i2]==TRUE)) - 1 else -1
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
    i1 <- targetrunA$from[i] + 2*prm.pt$rowFutureSpan 
    i2 <- targetrunA$to[i] - 2*prm.pt$rowFutureSpan
    if (i2>i1) targetrunA$FalseAlarmDensity[i] <- round(length(which(dt$rfAlarm[i1:i2]==TRUE))/(i2-i1+1),digits=3) else 0
  }

  # futureIgbtMax actual vs rfcutoff
  max_f_score <- which(rfcutPredict$f_score==max(rfcutPredict$f_score))[1]
  rfGTCutoffAlarm <- rfcutPredict$cutoff[max_f_score]
  # True negatives
  selected <- dt$futureIgbtMax<prm.pt$targetAlarmCutoff & dt$rfGTcut<rfGTCutoffAlarm
  plot(dt$futureIgbtMax[selected],dt$rfGTcut[selected], pch='.',col='green',
       main=sprintf('Actual futureIgbtMax vs rfGTcut=%.2f alarm %i minutes',
                    rfGTCutoffAlarm,round(prm.pt$rowFutureSpan/6)),
       xlab='actual futureIgbtMax',ylab='rfGTcut alarm',
       xlim=c(min(dt$predicted,dt$futureIgbtMax),max(dt$predicted,dt$futureIgbtMax)),
       ylim=c(-0.05, 1.05))
  # False negatives
  selected <- dt$futureIgbtMax>=prm.pt$targetAlarmCutoff & dt$rfGTcut<rfGTCutoffAlarm
  points(dt$futureIgbtMax[selected],dt$rfGTcut[selected], pch='.',col='black')
  # True positives
  selected <- dt$futureIgbtMax>=prm.pt$targetAlarmCutoff & dt$rfGTcut>=rfGTCutoffAlarm
  points(dt$futureIgbtMax[selected],dt$rfGTcut[selected], pch='.',col='red')
  # False positives
  selected <- dt$futureIgbtMax<prm.pt$targetAlarmCutoff & dt$rfGTcut>=rfGTCutoffAlarm
  points(dt$futureIgbtMax[selected],dt$rfGTcut[selected], pch='.',col='blue')
  obsCount <- rfcutPredict$truePositive[max_f_score] +
              rfcutPredict$trueNegative[max_f_score] +
              rfcutPredict$falsePositive[max_f_score] +
              rfcutPredict$falseNegative[max_f_score]
    
  legend('bottomleft',c(sprintf('False Positive (%.1f%%)',round(100*rfcutPredict$falsePositive[max_f_score]/obsCount,digits=1)),
                        sprintf('True Positive  (%.1f%%)',round(100*rfcutPredict$truePositive[max_f_score]/obsCount,digits=1)),
                        sprintf('True Negative  (%.1f%%)',round(100*rfcutPredict$trueNegative[max_f_score]/obsCount,digits=1)),
                        sprintf('False Negative (%.1f%%)',round(100*rfcutPredict$falseNegative[max_f_score]/obsCount,digits=1))),
         fill=c('blue','red','green','black'),cex=0.8)
  if (length(targetrunA$AdvancedAlarmTime[targetrunA$values=='highp' & targetrunA$highp>=prm.p$highCountForHist])>0) {
    hist(targetrunA$AdvancedAlarmTime[targetrunA$values=='highp' & targetrunA$highp>=prm.p$highCountForHist],
         breaks=(5+prm.pt$rowFutureSpan/6),
         xlab='Alarm Time in Advance of High Alarm Event (minutes)',
         ylab='High Alarm Event Count',
         main=sprintf('High %s Alarm Events from %s to %s',prm.pt$targetSource,substr(dt$time[1],1,10),substr(dt$time[nrow(dt)],1,10)))
  }
}

if (prm.p$stripchartInterval>0) {
  for (from in seq(1,nrow(dt),prm.p$stripchartInterval)) {
    to <- from + prm.p$stripchartInterval - 1
    predictStripchart(dt,prm.p$title, prm.pt$targetAlarmCutoff, from, to)
  }
}
if(!(prm.p$plotFilename=='')) dev.off()

if (sum(targetBelowCutoff,na.rm=T)>0 & sum(targetAboveCutoff,na.rm=T)>0) {
  targetrunA$roundAdvancedAlarmTime <- round(targetrunA$AdvancedAlarmTime)
  advancedAlarms <- data.frame(table(targetrunA$roundAdvancedAlarmTime[targetrunA$values=='highp']))
  colnames(advancedAlarms) <- c('time','count')
  advancedAlarms <- advancedAlarms[nrow(advancedAlarms):1,]
  advancedAlarms$prop <- round(advancedAlarms$count/sum(advancedAlarms$count),digits=3)
  advancedAlarms$cumprop <- round(cumsum(advancedAlarms$count/sum(advancedAlarms$count)),digits=3)
  cat('\nAdvanced Alarms Performance Table\n')
  print(advancedAlarms)
}

# write dt.diag file for Diagnostics
if (nchar(prm.p$diagFilename)>0) {
  cat('\nWriting predict diagnostics file with ',nrow(dt), 'rows and ',ncol(dt), 
      ' columns (csv file) : \n',prm.p$diagFilename,'\n')
  write.csv(dt,file=prm.p$diagFilename,row.names=FALSE)
}

# cat('\n\nEnding memory usage.\n')
# print(gc())
stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')

cat('Done.\n')
if (sink.number()>0) sink(file=NULL)
if (sink.number()>0) sink(file=NULL)
