#######################################################################################
# mainPredictTrain.R V3.0 - main routine for historian numeric prediction random forest training
# Ensign Energy Services Inc. retains all rights to this software
# FHS, April 30, 2017
#######################################################################################

# clear the memory
rm(list=ls())

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code master directory absolute address
library(randomForest)
library(caTools)
sourceDir <- "E:/Analytics/Rcode/190401_master/"
source(paste0(sourceDir,"source/predict/get.prm.predictTrain.R"))
source(paste0(sourceDir,"source/predict/dbuildPredict.R"))
source(paste0(sourceDir,"source/util/prm.decode.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/predict/predictTrain.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Get the training parameters
cat('\nLoading training parameters from file ',prmFilename,'\n\n')
prm.pt <- get.prm.predictTrain(prmFilename)

if (nchar(prm.pt$outputFilename)>0) {
  cat('Training Rig State Model and sending output to:\n',prm.pt$outputFilename,'\n')
  sink(file=prm.pt$outputFilename)
  options(width=132)
}

cat(prm.pt$version,'\nRun Started at : ',as.character(start.time),'\n')
cat('\nVerbatim listing of parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.pt$text)) { cat(prm.pt$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')
cat('\nProcessing for predictive random forest training begins here...\n')

# Load the training file
cat('\nReading historian training data file: \n',prm.pt$trainFilename)
# dt <- read.csv(prm.pt$trainFilename,nrow=150000)
dt <- read.csv(prm.pt$trainFilename)
cat('\nRead ',nrow(dt),' rows and ',ncol(dt),' columns.')

# check that source target column required for training is present
errorFlag <- FALSE
if (sum(names(dt) %in% prm.pt$targetSource) != 1) {
  cat('\n\nFATAL ERROR ... training target column ', prm.pt$targetSource, ' not found in training file..\n\n')
  errorFlag <- TRUE
}

# Rig column used for distinguishing multiple training chunks in a single training file
if (is.null(dt$Rig)==TRUE){
  cat('\n\nWARNING ... no Rig column detected, creating column with Rig="UNK"')
  dt$Rig <- "UNK"
}
if(sum(is.na(dt$Rig))>0) {
  cat("\nWARNING!!! missing ",sum(is.na(dt$Rig)), " 'Rig' values which are being set to ",dt$Rig[1],"\n")
  dt$Rig[is.na(dt$Rig)] <- dt$Rig[1]
}
dt$Rig <- as.factor(dt$Rig)

# Only keep the predictors needed for building and training
dt <- dt[,colnames(dt) %in% c('time','Rig',prm.pt$cpnames,prm.pt$target,
                              unique(prm.pt$standardUOM$uomColName))]
cat('\n\nRetaining ',ncol(dt),' input file columns for random forest training.\n')

# Loop through distinct DR training data blocks
for (l in levels(dt$Rig)) {
  dtb <- dt[dt$Rig==l,]
  dtb <- tryCatch({
    dbuildPredict(dtb,prm.pt)
  },
  error=function(e) {
    cat('\n\nERRORS in dbuildPredict, UNABLE TO PERFORM HISTORIAN PREDICTION TRAINING:\n',e$message,'\n\n')
    dtb <- NULL
  })
  if (!is.null(dtb)) {
    if (l==levels(dt$Rig)[1]) {
      dt1 <- dtb
    } else {
      dt1 <- rbind(dt1,dtb)
    }
    cat('\nProcessed dbuildPredict on rig=',l,' nrow(dt)=',nrow(dtb),' with a total of ',nrow(dt1),' observations.')
  } else { 
    errorFlag <- TRUE
  }
}

if (!errorFlag) {
  # check that calculated prediction target column required for training is present
  if (sum(names(dt1) %in% prm.pt$target) != 1) {
    cat('\n\nFATAL ERROR ... training target column ', prm.pt$target, ' not found in training file.\n\n')
    errorFlag <- TRUE
  }
}

# setup training dataset
if (!errorFlag) {
  # Only keep required predictors for training
  dt1 <- dt1[,c(prm.pt$tpnames,prm.pt$target)]
  for (i in 1:ncol(dt1)) {
    cat('\n',colnames(dt1)[i],' has ',sum(is.na(dt1[,i])),' missing values (',
        round(100*sum(is.na(dt1[,i]))/nrow(dt1),2),'%).')
  }
  # Only keep complete predictor observations for training
  cat('\nTraining data has a total of ',sum(!complete.cases(dt1)),
      ' incomplete cases with some missing values (',
      round(100*sum(!complete.cases(dt1))/nrow(dt1),2),'%).')
  dt1 <- dt1[complete.cases(dt1),]
  cat('\n',prm.pt$target,' has ',length(which(dt1[[prm.pt$target]]>=prm.pt$targetAlarmCutoff)),
      ' values above cutoff=',prm.pt$targetAlarmCutoff,
      ' and ',length(which(dt1[[prm.pt$target]]<prm.pt$targetAlarmCutoff)),' values below cutoff.')
  cat('\n',prm.pt$targetSource,' has ',length(which(dt1[[prm.pt$targetSource]]>=prm.pt$targetAlarmCutoff)),
      ' values above cutoff=',prm.pt$targetAlarmCutoff,
      ' and ',length(which(dt1[[prm.pt$targetSource]]<prm.pt$targetAlarmCutoff)),' values below cutoff.\n')
  
  cat('\n\nTraining data with complete observations and only required predictors has ',
      nrow(dt1),' rows and ',ncol(dt1),' columns')
  set.seed(123) # insures repeatable random sampling
  if (prm.pt$balanceTrainingSamples) { # stratified sampling
    cat('\nUsing balanced stratified sampling above and below alarm cutoff')
    selectAlarm <- which(dt1[[prm.pt$target]] >= prm.pt$targetAlarmCutoff)
    selectOK <- which(dt1[[prm.pt$target]] < prm.pt$targetAlarmCutoff)
    if (length(selectAlarm) >= prm.pt$trainSampleSize/2 & 
        length(selectOK) >= prm.pt$trainSampleSize/2) {
      trainIndex <- c(sample(selectAlarm,prm.pt$trainSampleSize/2),
                      sample(selectOK,prm.pt$trainSampleSize/2))
    } else if (length(selectAlarm) < prm.pt$trainSampleSize/2 & 
                length(selectOK) >= prm.pt$trainSampleSize/2) {
      trainIndex <- c(selectAlarm,
                      sample(selectOK,length(selectAlarm)))
    } else if (length(selectAlarm) >= prm.pt$trainSampleSize/2 & 
               length(selectOK) < prm.pt$trainSampleSize/2) {
      trainIndex <- c(sample(selectAlarm,length(selectOK)),
                      selectOK)
    } else {
      trainIndex <- c(sample(selectAlarm,min(length(selectAlarm),length(selectOK))),
                      sample(selectOK,min(length(selectAlarm),length(selectOK))))
    }
    train <- dt1[trainIndex,]
  } else { # No stratified sampling
    if(prm.pt$trainSampleSize >= nrow(dt1)) {
      train <- dt1
    } else {
      train <- dt1[sample(1:nrow(dt1),prm.pt$trainSampleSize),]
    }
  }

  cat('\nSelected ',nrow(train),' training observations with ',
      length(which(train[[prm.pt$target]]>=prm.pt$targetAlarmCutoff)),
      ' above alarm cutoff =',prm.pt$targetAlarmCutoff,' and ',
      length(which(train[[prm.pt$target]]<prm.pt$targetAlarmCutoff)),' below.\n\n')
  
  set.seed(456) # insures repeatable random forest results
  rf <- randomForest(train[,!colnames(train) %in% prm.pt$target],
                     train[,colnames(train) %in% prm.pt$target],
                     ntree=prm.pt$ntree,mtry=prm.pt$mtry,do.trace=prm.pt$do.trace)
  
  cat('\nRandom Forest Predictor Importance List\n')
  t <- as.data.frame(rf$importance[order(rf$importance,decreasing=T)])
  names(t) <- colnames(rf$importance)
  rownames(t) <- rownames(rf$importance)[order(rf$importance,decreasing=T)]
  t$prop <- t$IncNodePurity/sum(t$IncNodePurity)
  t$cumProp <- cumsum(t$IncNodePurity)/sum(t$IncNodePurity)
  print(t)
  
  # Look at votes from training data
  rf.all <- predict(rf,train[,!colnames(train) %in% prm.pt$target],predict.all=TRUE)
  train$rfGTcut <- NA
  train$rfGTcut <- apply(rf.all$individual,MARGIN=1,function(x) {sum(x>=prm.pt$targetAlarmCutoff)/length(x)})
  
  cat('\nWriting Random Forest Model (binary file) : \n',prm.pt$rfFilename,'\n\n')
  save(rf, file=prm.pt$rfFilename)
  
  if (nchar(prm.pt$importanceFilename)>0) { # sink file at end of program...
    cat('\nWriting Random Forest Predictor Importance List : \n',prm.pt$importanceFilename,'\n\n')
    t <- data.frame(paste0('[',rownames(rf$importance)[order(rf$importance,decreasing=T)],']'),
                    round(rf$importance[order(rf$importance,decreasing=T)],digits=1))
    names(t) <- c('name',colnames(rf$importance)[1])
    t$prop <- round(t$IncNodePurity/sum(t$IncNodePurity),digits=4)
    t$cumProp <- round(cumsum(t$IncNodePurity)/sum(t$IncNodePurity),digits=4)
  }
}

stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')
cat('Done.\n')

if (!errorFlag & nchar(prm.pt$importanceFilename)>0) {
  sink(file=prm.pt$importanceFilename)
  print(t)
}
if (sink.number()>0) sink(file=NULL)
if (sink.number()>0) sink(file=NULL)
