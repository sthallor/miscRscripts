#######################################################################################
# Rig Classify Train V3.1 - main routine for random forest training
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 28, 2019
#######################################################################################

if (sink.number()>0) sink(file=NULL)
cat('\nStarting memory usage.\n')
print(gc())
start.time <- Sys.time()

# Source code master directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/slope.R"))
source(paste0(sourceDir,"source/util/runmeanA.R"))
source(paste0(sourceDir,"source/classify/get.prm.t.R"))
source(paste0(sourceDir,"source/classify/dbuild.R"))
source(paste0(sourceDir,"source/classify/makeEigen.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/classify/train.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Get the training parameters
cat('\nLoading training parameters from file ',prmFilename,'\n\n')
prm.t <- get.prm.t(prmFilename)

if (nchar(prm.t$outputFilename)>0) {
  cat('Training Rig State Model and sending output to:\n',prm.t$outputFilename,'\n')
  sink(file=prm.t$outputFilename)
  options(width=132)
}

cat(prm.t$version,'\nRun Started at : ',as.character(start.time),'\n')
cat('\nVerbatim listing of parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.t$text)) { cat(prm.t$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')
cat('\nProcessing for random forest training begins here...\n')

# Load the training file
cat('\nReading rig state training data file: \n',prm.t$trainFilename)
dt <- read.csv(prm.t$trainFilename)
cat('\nRead ',nrow(dt),' rows and ',ncol(dt),' columns.')

# check that target column required for training is present
errorFlag <- FALSE
if (sum(names(dt) %in% prm.t$target) != 1) {
  cat('\n\nFATAL ERROR ... training target column ', prm.t$target, ' not found in training file..\n\n')
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
dt <- dt[,colnames(dt) %in% c(prm.t$timeColName,'Rig',prm.t$cp$name,prm.t$tpnames,
                              prm.t$target,unique(prm.t$standardUOM$uomColName))]
cat('\n\nRetaining ',ncol(dt),' input file columns for random forest training.\n')

# create eigenvalues and eigenvectors if any are requested.
if (sum(prm.t$cp$eigencount)>0) {
  eigenRes <- makeEigen(dt,prm.t)
} else {
  eigenRes <- NULL
}

# Loop through distinct DR training data blocks
for (l in levels(dt$Rig)) {
  train <- dt[dt$Rig==l,]
  dtb <- tryCatch({
    dbuild(train,prm.t,eigenRes)
  },
  error=function(e) {
    cat('\n\nERRORS in dbuild, UNABLE TO PERFORM RIG STATE TRAINING:\n',e$message,'\n\n')
    errorFlag <<- TRUE
    dtb <<- NULL
  })
  if (!is.null(dtb)) {
    if (l==levels(dt$Rig)[1]) {
      dt1 <- dtb
    } else {
      dt1 <- rbind(dt1,dtb)
    }
    if (prm.t$verbose) cat('\n')
    cat('\nProcessed dbuild on rig=',l,' nrow(dt)=',nrow(dtb),' with a total of ',nrow(dt1),' observations.\n')
    if (prm.t$verbose) cat(paste(rep('-',80),collapse=''),'\n')
  }
}

# If option selected, doubles training dataset with missing value to EDR_RotaryTorque, FHS 3/7/2019
if (prm.t$missingTorque) {
  if(!is.null(dt1$EDR_RotaryTorque)) {
    rowCount <- nrow(dt1)
    dt1 <- rbind(dt1,dt1)
    dt1$EDR_RotaryTorque[1:rowCount] <- prm.t$missingTorqueValue
    cat('\n\nOption to include missing EDR_RotaryTorque values in training model selected.')
    cat('\nModel size has been doubled to ',nrow(dt1),' rows.\n\n')
  } else {
    cat('\n\n\nWARNING... there is no EDR_RotaryTorque in training dataset but missing value options selected')
    cat ('\nTraining dataset EDR_Torque values constant, adds nothing to model.')
    cat ('\nTherefore missing value option ignored.\n\n\n')
  }
}

if (!errorFlag) {
  
  ##################################
  # DIAGNOSTIC switches located here
  prm.t$trainProportion <- 1.0    # use 1.0 to skip test dataset
  ##################################
  
  # sets up individual data points for random forest prediction training
  X.train <- dt1[complete.cases(dt1[prm.t$tpnames]),(colnames(dt1) %in% prm.t$tpnames)]
  y.train <- dt1[complete.cases(dt1[prm.t$tpnames]),(colnames(dt1) %in% prm.t$target)]
  # reset factors, because small training sets cannot have empty categorization classes
  y.train <- as.character(y.train)
  y.train <- as.factor(y.train)
  
  if (prm.t$trainProportion < 1.0) {
    library(caTools)
    train_rows <- sample.split(y.train,SplitRatio=prm.t$trainProportion)
    X.test <- X.train[as.logical(1-train_rows),]
    y.test <- y.train[as.logical(1-train_rows)]
    X.train <- X.train[as.logical(train_rows),]
    y.train <- y.train[as.logical(train_rows)] 
  }
  
  cat('\nRandom Forest training dataset has ',nrow(X.train),
      ' training examples with ',ncol(X.train),' predictor feature variables.\n\n')

  cat('The quantities of target values are as follows:\n')
  print(table(y.train))
  
  ID.train <- dt1[complete.cases(dt1[prm.t$predictors]),'ID']
  library(randomForest)

  cat('\nBuilding random forest model from training dataset\n')
  set.seed(160917)
  # rf <- randomForest(X.train,y.train,ntree=prm.t$ntree,do.trace=prm.t$dotrace) # Production settings 3/6/2019
  
  if (prm.t$rfchunks>1) cat('\nSplitting tree build into ',prm.t$rfchunks,' chunks of ',
                           prm.t$ntree,' trees for a total of',prm.t$ntree * prm.t$rfchunks,' trees.\n')
  
  for (rfi in 1:prm.t$rfchunks) {
    if (prm.t$rfchunks>1) cat('\nWorking on chunk ',rfi,' out of ',prm.t$rfchunks,' total.\n')
    rftemp <- randomForest(X.train,y.train,ntree=prm.t$ntree,do.trace=prm.t$dotrace)
    if (rfi==1) {
      rf <- rftemp
    } else {
      rf <- combine(rf,rftemp)
    }
  }
  rf$votes <- rf$votes/prm.t$rfchunks

  cat('\nRandom Forest Confusion Matrix (classification counts)
      rows=training target actual classification categories
      columns=predicted classification categories\n\n')
  print(rf$confusion)
  
  cat('\nRandom Forest Decision Tree Classification Proportional Votes
      rows=predicted classification categories
      columns=random forest decision tree voting categories\n\n')
  df <- aggregate(.~y,data=cbind(y=rf$predicted,as.data.frame(rf$votes)),
                  FUN=function(x){mean(x)})
  rownames(df) <- df$y
  df <- as.matrix(df[,2:ncol(df)])
  df <- cbind(df,vote.confidence=rowSums(df * diag(ncol(df))))
  df <- cbind(df,count=as.vector(table(rf$predicted)))
  df[,1:(ncol(df)-1)] <- round(df[,1:(ncol(df)-1)],digits=3)
  print(df)
  
  cat('\nOverall Random Forest Classification Training Vote Confidence=',
      round(sum(df[,(ncol(df)-1)]*df[,ncol(df)])/sum(df[,ncol(df)]),digits=3),'\n')

  cat('\nRandom Forest ',nrow(rf$importance),' Predictors Importance List\n')
  t <- as.data.frame(rf$importance[order(rf$importance,decreasing=T)])
  names(t) <- colnames(rf$importance)
  rownames(t) <- rownames(rf$importance)[order(rf$importance,decreasing=T)]
  t$prop <- t$MeanDecreaseGini/sum(t$MeanDecreaseGini)
  t$cumProp <- cumsum(t$MeanDecreaseGini)/sum(t$MeanDecreaseGini)
  print(t)
  
  cat('\nTraining dataset accuracy=',round(sum(rf$y==rf$predicted)/length(rf$y),digits=3),
      ' with ',length(rf$y),' observations')
  
  if (prm.t$trainProportion < 1.0) {
    yhat.test <- predict(rf,X.test)
    cat('\nTest     dataset accuracy=',round(sum(y.test==yhat.test)/length(y.test),digits=3),
        ' with ',length(y.test),' observations')
  }
  
  cat('\n\nWriting Random Forest Model (binary file) : \n',prm.t$rfFilename,'\n\n')
  save(rf, file=prm.t$rfFilename)
  
  if (sum(prm.t$cp$eigencount)>0) {
    cat('\n\nWriting Eigen Vector Model (binary file) : \n',prm.t$evFilename,'\n\n')
    save(eigenRes, file=prm.t$evFilename)
  }
  
  cat('\nWriting Random Forest Predictor Importance List : \n',prm.t$importanceFilename,'\n\n')
  t <- data.frame(paste0('[',rownames(rf$importance)[order(rf$importance,decreasing=T)],']'),
                 round(rf$importance[order(rf$importance,decreasing=T)],digits=1))
  names(t) <- c('name',colnames(rf$importance)[1])
  t$prop <- round(t$MeanDecreaseGini/sum(t$MeanDecreaseGini),digits=4)
  t$cumProp <- round(cumsum(t$MeanDecreaseGini)/sum(t$MeanDecreaseGini),digits=4)
}

cat('\nEnding memory usage.\n')
print(gc())
stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')

cat('Done.\n')
if (!errorFlag & nchar(prm.t$importanceFilename)>0) {
  sink(file=prm.t$importanceFilename)
  print(t)
}
if (sink.number()>0) sink(file=NULL)
if (sink.number()>0) sink(file=NULL)
