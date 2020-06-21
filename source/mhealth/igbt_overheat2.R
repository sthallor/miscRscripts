#######################################################################################
# igbt_tempOverheat2.R - predicting future IGBT max temperature
# record random forest vote that it exceeds 100C
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 2, 2017
#######################################################################################
start.time <- Sys.time()

library(caTools)
library(randomForest)
sourceDir <- "/home/fhs/Rcode/Ensign/"
source(paste0(sourceDir,"mhealth/rfcutplot.R"))


dir <- '/home/fhs/LargeDatasets/Ensign/170222_MachineHealth/'
# filename <- '170222_DR156_IGBT_temp_Jan2017_clean.csv'
# filename <- '160512_DR156_IGBT_temp_regression_A_clean1.csv'
filename <- '170222_DR156_IGBT_temp_All2016_clean.csv'
dt <- read.csv(paste0(dir,filename), nrow=-1)
cat('\nRead file ',filename,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')

dt$time <- as.character(dt$time)
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
for (i in 2:ncol(dt)) dt[,i] <- as.numeric(dt[,i])

# calculates timespan between observations
dt$timespan <- 0
dt$timespan[1] <- as.numeric(difftime(dt$time[2],dt$time[1],units='secs'))
dt$timespan[2:(nrow(dt)-1)] <- as.numeric(difftime(dt$time[3:nrow(dt)],
                                                   dt$time[1:(nrow(dt)-2)],units='secs'))/2
dt$timespan[nrow(dt)] <- as.numeric(difftime(dt$time[nrow(dt)],
                                             dt$time[(nrow(dt)-1)],units='secs'))
dt$timespan[dt$timespan<1] <- 1 # minimum allowed timespan value which is used as denominator


# Sets up future IGBT temperature classifications
rowFutureOffset <- 0 # offset 5 minutes into the future for start of span
rowFutureSpan <- 120 # timespan is 10 minutes from offset into the future
dt$futureIgbtMax <- NA
dt$futureIgbtMax[1:(nrow(dt)-rowFutureOffset)] <- 
  runmax(dt$igbt_temp[(rowFutureOffset+1):nrow(dt)],k=rowFutureSpan,align='left')

dt$futureIgbt <- NA
dt$futureIgbt[dt$futureIgbtMax<100] <- 'coolf'
dt$futureIgbt[dt$futureIgbtMax>=100] <- 'hotf'
dt$futureIgbt <- factor(dt$futureIgbt,levels=list(coolf='coolf',hotf='hotf'))

cat('\nFrom',round(rowFutureOffset/6), ' to ',round((rowFutureOffset+rowFutureSpan)/6),
    ' minutes in the future IGBT max temperature classifications in dataset\n')
print(table(dt$futureIgbt))

# # setup present IGBT temperature classifications
dt$presentIgbt <- NA
dt$presentIgbt[dt$igbt_temp<100] <- 'coolp'
dt$presentIgbt[dt$igbt_temp>=100] <- 'hotp'
dt$presentIgbt <- factor(dt$presentIgbt,levels=list(coolp='coolp',hotp='hotp'))

cat('\nPresent actual IGBT temperature classifications in dataset\n')
print(table(dt$presentIgbt))

cat('\n Comparison between present and future IGBT temperature classifications\n')
print(table(dt$presentIgbt,dt$futureIgbt))

# Calculated predictors begin here
for (runmeanCount in c(120)) {
  for (runSDCount in c(120)) {
    dt$igbt_tempRunMean <- runmean(dt$igbt_temp,k=runmeanCount,align='right')
    dt$igbt_tempRunSD <- runsd(dt$igbt_temp,k=runSDCount,align='right')
    # dt$igbt_tempRunMax <- runmax(dt$igbt_temp,k=runSDCount,align='right')
    
    # Cumulative abs delta block height for rowSpan observations to the right
    temp <- dt$block_height
    temp[1] <- 0
    temp[2:nrow(dt)] <- abs(dt$block_height[2:nrow(dt)]-dt$block_height[1:(nrow(dt)-1)])
    temp[is.na(temp)] <- 0
    temp <- cumsum(temp)
    dt$deltaBlockHeightAbs <- 0
    dt$deltaBlockHeightAbs[1:runSDCount] <- temp[1:runSDCount]
    dt$deltaBlockHeightAbs[(runSDCount+1):nrow(dt)] <- temp[(runSDCount+1):nrow(dt)]-temp[1:(nrow(dt)-runSDCount)]
    
    dt$dc_bus_voltageRunMean <- runmean(dt$dc_bus_voltage,k=runmeanCount,align='right')
    dt$dc_bus_voltageRunSD <- runsd(dt$dc_bus_voltage,k=runSDCount,align='right') # 
    
    dt$currentRunSD <- runsd(dt$current,k=runSDCount,align='right')
    dt$currentRunMean <- runmean(dt$current,k=runmeanCount,align='right') # 
    
    dt$hookloadRunSD <- runsd(dt$hookload,k=runSDCount,align='right')
    dt$hookloadRunMean <- runmean(dt$hookload,k=runmeanCount,align='right')
    
    # Power injected into system
    dt$kwatts <- dt$current * dt$dc_bus_voltage/1000
    dt$kwattsRunMean <- runmean(dt$kwatts,k=runmeanCount,align='right')
    dt$kwattsRunSD <- runsd(dt$kwatts,k=runSDCount,align='right')
    
    dt$frequencyRunMean <- runmean(dt$frequency,k=runmeanCount,align='right') # 
    dt$frequencyRunSD <- runsd(dt$frequency,k=runSDCount,align='right') # 
    
    dt$torqueRunMean <- runmean(dt$torque,k=runmeanCount,align='right') # 
    dt$torqueRunSD <- runsd(dt$torque,k=runSDCount,align='right') # 
    
    dt$speedRunMean <- runmean(dt$speed,k=runmeanCount,align='right') #
    dt$speedRunSD <- runsd(dt$speed,k=runSDCount,align='right') #
    
    dt$output_voltageRunMean <- runmean(dt$output_voltage,k=runmeanCount,align='right') #
    dt$output_voltageRunSD <- runsd(dt$output_voltage,k=runSDCount,align='right') #
    
    ###################################################################################
    # Sets up random forest training here
    trainSampleSize <- 75000
    testSampleSize <- 200000
    
    (regVar <- c('igbt_tempRunMean','igbt_tempRunSD','igbt_temp',
                 'deltaBlockHeightAbs',
                 'kwattsRunMean','kwattsRunSD',
                 'dc_bus_voltageRunMean','currentRunSD','hookloadRunSD','hookloadRunMean',
                 'dc_bus_voltageRunSD','currentRunMean',
                 'frequencyRunMean','frequencyRunSD',
                 'torqueRunMean','torqueRunSD',
                 'speedRunMean','speedRunSD',
                 'output_voltageRunMean','output_voltageRunSD'))
    
    
    set.seed(123)
    # trainIndex <- 1:(nrow(dt) - testSampleSize) # test dataset at end
    trainIndex <- (testSampleSize+1):nrow(dt) # test dataset at begining
    t1 <- which(dt$futureIgbtMax[trainIndex] >= 100)
    t2 <- which(dt$futureIgbtMax[trainIndex] < 100)
    if(length(t1)>trainSampleSize/2) t1 <- sample(t1,trainSampleSize/2) else t1 <- t1
    if(length(t2)>trainSampleSize/2) t2 <- sample(t2,trainSampleSize/2) else t2 <- t2
    trainIndex <- c(t1,t2)
    train <- dt[trainIndex,]
    
    # test <- dt[(nrow(dt)-testSampleSize+1):nrow(dt),] # test dataset at end
    test <- dt[1:testSampleSize,] # test dataset at begining
    cat('\n\nTotal training sample size ',trainSampleSize,' selected ',length(t1),' observations above 100C and ',
        length(t2),' observations below 100C')
    
    train_select <- complete.cases(train[c(regVar,'futureIgbtMax')])
    X.train <- train[train_select,(colnames(train) %in% regVar)]
    y.train <- train[train_select,(colnames(train) %in% 'futureIgbtMax')]
    
    cat('\n\nRandom forest training with ',nrow(X.train),' complete observations and ',
        ncol(X.train),' predictors.\n\n')
    
    rf <- randomForest(X.train,y.train,ntree=300,mtry=3,do.trace=25)

    cat('\nRandom Forest Predictor Importance List\n')
    t <- as.data.frame(rf$importance[order(rf$importance,decreasing=T)])
    names(t) <- colnames(rf$importance)
    rownames(t) <- rownames(rf$importance)[order(rf$importance,decreasing=T)]
    t$prop <- t$IncNodePurity/sum(t$IncNodePurity)
    t$cumProp <- cumsum(t$IncNodePurity)/sum(t$IncNodePurity)
    print(t)
    
    # Look at votes from training data
    rf.all <- predict(rf,X.train,predict.all=TRUE)
    train$rfGT100 <- NA
    train$rfGT100[train_select] <- apply(rf.all$individual,MARGIN=1,function(x) {sum(x>=100)/length(x)})
    
    train_select1 <- train_select & train$igbt_temp < 100 
    rfcutTrain <- data.frame(cutoff=seq(0,1.00,.01))
    rfcutTrain$trueNegative <- apply(as.matrix(rfcutTrain$cutoff,ncol=1),MARGIN=1,function(x) {
      table(train$futureIgbt[train_select1],train$rfGT100[train_select1]>x)[1,1]})
    rfcutTrain$falseNegative <- apply(as.matrix(rfcutTrain$cutoff,ncol=1),MARGIN=1,function(x) {
      table(train$futureIgbt[train_select1],train$rfGT100[train_select1]>x)[2,1]})
    rfcutTrain$falsePositive <- apply(as.matrix(rfcutTrain$cutoff,ncol=1),MARGIN=1,function(x) {
      if (sum(train$rfGT100[train_select1]>x)>0) table(train$futureIgbt[train_select1],train$rfGT100[train_select1]>x)[1,2] else 1})
    rfcutTrain$truePositive <- apply(as.matrix(rfcutTrain$cutoff,ncol=1),MARGIN=1,function(x) {
      if (sum(train$rfGT100[train_select1]>x)>0) table(train$futureIgbt[train_select1],train$rfGT100[train_select1]>x)[2,2] else 0})
    rfcutTrain$sensitivity <- rfcutTrain$truePositive/(rfcutTrain$truePositive+rfcutTrain$falseNegative)
    rfcutTrain$specificity <- rfcutTrain$trueNegative/(rfcutTrain$trueNegative+rfcutTrain$falsePositive)
    rfcutTrain$posPredValue <- rfcutTrain$truePositive/(rfcutTrain$truePositive+rfcutTrain$falsePositive)
    rfcutTrain$negPredValue <- rfcutTrain$trueNegative/(rfcutTrain$trueNegative+rfcutTrain$falseNegative)
    rfcutTrain$f_score <- 2*(rfcutTrain$posPredValue*rfcutTrain$sensitivity)/(rfcutTrain$posPredValue+rfcutTrain$sensitivity)
    rfcutTrain$f_score[is.na(rfcutTrain$f_score)] <- 0
    rfcutTrain$f_score2 <- 2*(rfcutTrain$posPredValue*rfcutTrain$sensitivity^2)/(rfcutTrain$posPredValue+rfcutTrain$sensitivity^2)
    rfcutTrain$f_score2[is.na(rfcutTrain$f_score2)] <- 0
    rfcutplot(rfcutTrain,sprintf('Train RF Vote Cutoff from %i to %i minute future IGBT temp',
                                 round(rowFutureOffset/6),round((rowFutureOffset+rowFutureSpan)/6)))
    
    cat('\nTraining Data Optimum f_score rf tree threshold cutoff for 100C runMeanCount=',runmeanCount,' runSDCount=',runSDCount,'\n')
    print(rfcutTrain[rfcutTrain$f_score==max(rfcutTrain$f_score),])
    
    
    cat('\nExamining validation test dataset with ',nrow(test),' total observations.\n')
    rf.all <- predict(rf,test[complete.cases(test[regVar]),regVar],predict.all=TRUE)
    
    test$predicted <- NA
    test$predicted[complete.cases(test[regVar])] <- rf.all$aggregate

    # Now look at individual rf tree determinations
    test$rfGT100 <- NA
    test$rfGT100[complete.cases(test[regVar])] <- apply(rf.all$individual,MARGIN=1,function(x) {sum(x>=100)/length(x)})

    # Optimize probability threshold for igbt_temp values below critical 100C
    test_select <- test$igbt_temp < 100 & complete.cases(test[regVar])
    rfcutTest <- data.frame(cutoff=seq(0,1.00,.01))
    rfcutTest$trueNegative <- apply(as.matrix(rfcutTest$cutoff,ncol=1),MARGIN=1,function(x) {
      table(test$futureIgbt[test_select],test$rfGT100[test_select]>x)[1,1]})
    rfcutTest$falseNegative <- apply(as.matrix(rfcutTest$cutoff,ncol=1),MARGIN=1,function(x) {
      table(test$futureIgbt[test_select],test$rfGT100[test_select]>x)[2,1]})
    rfcutTest$falsePositive <- apply(as.matrix(rfcutTest$cutoff,ncol=1),MARGIN=1,function(x) {
      if (sum(test$rfGT100[test_select]>x)>0) table(test$futureIgbt[test_select],test$rfGT100[test_select]>x)[1,2] else 1})
    rfcutTest$truePositive <- apply(as.matrix(rfcutTest$cutoff,ncol=1),MARGIN=1,function(x) {
      if (sum(test$rfGT100[test_select]>x)>0) table(test$futureIgbt[test_select],test$rfGT100[test_select]>x)[2,2] else 0})
    rfcutTest$sensitivity <- rfcutTest$truePositive/(rfcutTest$truePositive+rfcutTest$falseNegative)
    rfcutTest$specificity <- rfcutTest$trueNegative/(rfcutTest$trueNegative+rfcutTest$falsePositive)
    rfcutTest$posPredValue <- rfcutTest$truePositive/(rfcutTest$truePositive+rfcutTest$falsePositive)
    rfcutTest$negPredValue <- rfcutTest$trueNegative/(rfcutTest$trueNegative+rfcutTest$falseNegative)
    rfcutTest$f_score <- 2*(rfcutTest$posPredValue*rfcutTest$sensitivity)/(rfcutTest$posPredValue+rfcutTest$sensitivity)
    rfcutTest$f_score[is.na(rfcutTest$f_score)] <- 0
    rfcutTest$f_score2 <- 2*(rfcutTest$posPredValue*rfcutTest$sensitivity^2)/(rfcutTest$posPredValue+rfcutTest$sensitivity^2)
    rfcutTest$f_score2[is.na(rfcutTest$f_score2)] <- 0
    rfcutplot(rfcutTest,sprintf('Test RF Vote Cutoff from %i to %i minute future IGBT temp',
                                 round(rowFutureOffset/6),round((rowFutureOffset+rowFutureSpan)/6)))
    cat('\nTest Data Optimum f_score rf tree threshold cutoff for 100C runMeanCount=',runmeanCount,' runSDCount=',runSDCount,'\n')
    print(rfcutTest[rfcutTest$f_score==max(rfcutTest$f_score) | rfcutTest$f_score2==max(rfcutTest$f_score2),])
  }
}
# This value used in plot, FHS Feb 24, 2017
rfGT100CutoffAlarm <- rfcutTest$cutoff[rfcutTest$f_score==max(rfcutTest$f_score)]
rfGT100CutoffWarning <- rfcutTest$cutoff[rfcutTest$f_score2==max(rfcutTest$f_score2)]
  
stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='mins')),digits=1),' minutes.\n')
