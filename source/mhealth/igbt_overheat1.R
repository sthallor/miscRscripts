#######################################################################################
# igbt_tempOverheat1.R - predicting whether IGBT temperature will increase
# fixed class over 100C
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 22, 2017
#######################################################################################

start.time <- Sys.time()

library(caTools)
library(randomForest)

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filename <- '160512_DR156_IGBT_temp_regression_A_clean.csv'
dt <- read.csv(paste0(dir,filename), nrow=-1)
cat('\nRead file ',filename,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')

# shorten the column names
colnames(dt)[2] <- substr(colnames(dt)[2],26,nchar(colnames(dt)[2]))
colnames(dt)[3:10] <- substr(colnames(dt)[3:10],30,nchar(colnames(dt)[3:10]))

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
rowFutureOffset <- 30 # offset 5 minutes into the future for start of span
rowFutureSpan <- 60 # timespan is 10 minutes from offset into the future
dt$futureIgbtMax <- NA
dt$futureIgbtMax[1:(nrow(dt)-rowFutureOffset)] <- runmax(dt$igbt_temp[(rowFutureOffset+1):nrow(dt)],k=rowFutureSpan,align='left')

# dt$futureIgbt <- NA
# dt$futureIgbt[dt$futureIgbtMax<50] <- 'coolf'
# dt$futureIgbt[dt$futureIgbtMax>50] <- 'warmf'
# dt$futureIgbt[dt$futureIgbtMax>80] <- 'hotf'
# dt$futureIgbt[dt$futureIgbtMax>100] <- 'very_hotf'
# dt$futureIgbt <- factor(dt$futureIgbt,levels=list(coolf='coolf',warmf='warmf',hotf='hotf',very_hotf='very_hotf'))
dt$futureIgbt <- NA
dt$futureIgbt[dt$futureIgbtMax<100] <- 'coolf'
dt$futureIgbt[dt$futureIgbtMax>=100] <- 'hotf'
dt$futureIgbt <- factor(dt$futureIgbt,levels=list(coolf='coolf',hotf='hotf'))



cat('\nFrom',round(rowFutureOffset/6), ' to ',round((rowFutureOffset+rowFutureSpan)/6),
    ' minutes in the future IGBT max temperature classifications in dataset\n')
print(table(dt$futureIgbt))

# # setup present IGBT temperature classifications
# dt$presentIgbt <- NA
# dt$presentIgbt[dt$igbt_temp<50] <- 'coolp'
# dt$presentIgbt[dt$igbt_temp>50] <- 'warmp'
# dt$presentIgbt[dt$igbt_temp>80] <- 'hotp'
# dt$presentIgbt[dt$igbt_temp>100] <- 'very_hotp'
# dt$presentIgbt <- factor(dt$presentIgbt,levels=list(coolp='coolp',warmp='warmp',hotp='hotp',very_hotp='very_hotp'))
# setup present IGBT temperature classifications
dt$presentIgbt <- NA
dt$presentIgbt[dt$igbt_temp<100] <- 'coolp'
dt$presentIgbt[dt$igbt_temp>=100] <- 'hotp'
dt$presentIgbt <- factor(dt$presentIgbt,levels=list(coolp='coolp',hotp='hotp'))



cat('\nPresent actual IGBT temperature classifications in dataset\n')
print(table(dt$presentIgbt))

cat('\n Comparison between present and future IGBT temperature classifications\n')
print(table(dt$presentIgbt,dt$futureIgbt))


# Calculated predictors begin here

dt$igbt_tempRunMean60 <- runmean(dt$igbt_temp,k=60,align='right')
dt$igbt_tempRunSD90 <- runsd(dt$igbt_temp,k=90,align='right')
# Cumulative abs delta block height for rowSpan observations to the right
temp <- dt$block_height
temp[1] <- 0
temp[2:nrow(dt)] <- abs(dt$block_height[2:nrow(dt)]-dt$block_height[1:(nrow(dt)-1)])
temp[is.na(temp)] <- 0
temp <- cumsum(temp)
dt$deltaBlockHeightAbs90 <- 0
dt$deltaBlockHeightAbs90[1:90] <- temp[1:90]
dt$deltaBlockHeightAbs90[91:nrow(dt)] <- temp[91:nrow(dt)]-temp[1:(nrow(dt)-90)]

dt$dc_bus_voltageRunMean60 <- runmean(dt$dc_bus_voltage,k=60,align='right')
dt$currentRunSD90 <- runsd(dt$current,k=90,align='right')
dt$hookloadRunSD90 <- runsd(dt$hookload,k=90,align='right')
dt$hookloadRunMean60 <- runmean(dt$hookload,k=60,align='right')

# Power injected into system
dt$kwatts <- dt$current * dt$dc_bus_voltage/1000
dt$kwattsRunMean60 <- runmean(dt$kwatts,k=60,align='right')
dt$kwattsRunSD90 <- runsd(dt$kwatts,k=90,align='right')


# rowSpan <- 30
# dt$hookloadRunMean <- runmean(dt$hookload,k=rowSpan,align='right')
# dt$hookloadRunSD <- runsd(dt$hookload,k=rowSpan,align='right')
# dt$torqueRunMean <- runmean(dt$torque,k=rowSpan,align='right')
# dt$torqueRunSD <- runsd(dt$torque,k=rowSpan,align='right')
# dt$kwatts <- dt$current * dt$dc_bus_voltage/1000
# dt$kwattsRunMean <- runmean(dt$kwatts,k=rowSpan,align='right')
# dt$kwattsRunSD <- runsd(dt$kwatts,k=rowSpan,align='right')
# dt$speedRunMean <- runmean(dt$speed,k=rowSpan,align='right')
# dt$speedRunSD <- runsd(dt$speed,k=rowSpan,align='right')
# dt$output_voltageRunMean <- runmean(dt$output_voltage,k=rowSpan,align='right')
# dt$output_voltageRunSD <- runsd(dt$output_voltage,k=rowSpan,align='right')
# dt$frequencyRunMean <- runmean(dt$frequency,k=rowSpan,align='right')
# dt$frequencyRunSD <- runsd(dt$frequency,k=rowSpan,align='right')
# dt$currentRunMean <- runmean(dt$current,k=rowSpan,align='right')
# dt$currentRunSD <- runsd(dt$current,k=rowSpan,align='right')
# dt$dc_bus_voltageRunMean <- runmean(dt$dc_bus_voltage,k=rowSpan,align='right')
# dt$dc_bus_voltageRunSD <- runsd(dt$dc_bus_voltage,k=rowSpan,align='right')

###################################################################################
# Sets up random forest training here
sampleSize <- 100000

# (regVar <- c('dc_bus_voltage','dc_bus_voltageRunMean','dc_bus_voltageRunSD',
#              'output_voltage','output_voltageRunMean','output_voltageRunSD',
#              'hookload','hookloadRunMean','hookloadRunSD',
#              'torque','torqueRunMean','torqueRunSD',
#              'igbt_temp','igbt_tempRunMean','igbt_tempRunSD',
#              'kwattsRunMean','kwattsRunSD',
#              'speedRunMean','speedRunSD',
#              'frequencyRunMean','frequencyRunSD',
#              'current','currentRunMean','currentRunSD',
#              'deltaBlockHeightAbs'))

(regVar <- c('igbt_tempRunMean60','igbt_tempRunSD90','igbt_temp',
             'deltaBlockHeightAbs90',
             'kwattsRunMean60','kwattsRunSD90',
             'dc_bus_voltageRunMean60','currentRunSD90','hookloadRunSD90','hookloadRunMean60'))


set.seed(123)
trainIndex <- 1:sampleSize
# This is setup to undersample the 'coolf' training dataset - FHS Feb 22, 2017
t1 <- which(dt$futureIgbt[trainIndex] == 'hotf')
t2 <- which(dt$futureIgbt[trainIndex] == 'coolf')
if (length(t2)>length(t1)) t2 <- sample(t2,length(t1))
trainIndex <- c(t1,t2)
train <- dt[trainIndex,]
test <- dt[(sampleSize+1):nrow(dt),]

X.train <- train[complete.cases(train[c(regVar,'futureIgbt')]),(colnames(train) %in% regVar)]
y.train <- train[complete.cases(train[c(regVar,'futureIgbt')]),(colnames(train) %in% 'futureIgbt')]
y.train <- as.factor(y.train)

cat('\n\nRandom forest training with ',nrow(X.train),' observations and ',
    ncol(X.train),' predictors.\n\n')

rf <- randomForest(X.train,y.train,ntree=500,do.trace=100)

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

cat('\nRandom Forest Predictor Importance List\n')
t <- as.data.frame(rf$importance[order(rf$importance,decreasing=T)])
names(t) <- colnames(rf$importance)
rownames(t) <- rownames(rf$importance)[order(rf$importance,decreasing=T)]
t$prop <- t$MeanDecreaseGini/sum(t$MeanDecreaseGini)
t$cumProp <- cumsum(t$MeanDecreaseGini)/sum(t$MeanDecreaseGini)
print(t)

# Look at the training accuracy for predicting change in temperature category
train$predicted <- NA
train$predicted[complete.cases(train[c(regVar,'futureIgbt')])] <- rf$predicted
train$predicted <- factor(train$predicted)
levels(train$predicted) <- c('coolfp','hotfp')
levels(train$presentIgbt) <- levels(train$futureIgbt)
selectChange <- train$presentIgbt != train$futureIgbt
tbl <- table(train$futureIgbt[selectChange],train$predicted[selectChange])
cat('\n\n')
print(cbind(as.matrix(tbl),accuracy=round(rowSums(tbl*diag(nrow(tbl)))/rowSums(tbl),digits=3)))
cat('The overall training data accuracy for predicting a change in temperature state is ',
    round(sum(tbl*diag(nrow(tbl))/sum(tbl)),digits=3),'\n')

cat('\nExamining validation test dataset with ',nrow(test),' total observations.\n')
test$predicted <- NA
test$predicted[complete.cases(test[regVar])] <- predict(rf,test[complete.cases(test[regVar]),regVar])
test$predicted <- factor(test$predicted)
# levels(test$predicted) <- c('coolfp','warmfp','hotfp','very_hotfp')
levels(test$predicted) <- c('coolfp','hotfp')
tbl <- table(test$futureIgbt,test$predicted)
print(cbind(as.matrix(tbl),accuracy=round(rowSums(tbl*diag(nrow(tbl)))/rowSums(tbl),digits=3)))
cat('The overall test data accuracy for predicting the temperature state is ',round(sum(tbl*diag(nrow(tbl))/sum(tbl)),digits=3),'\n\n')

levels(test$presentIgbt) <- levels(test$futureIgbt)
selectChange <- test$presentIgbt != test$futureIgbt
tbl <- table(test$futureIgbt[selectChange],test$predicted[selectChange])
print(cbind(as.matrix(tbl),accuracy=round(rowSums(tbl*diag(nrow(tbl)))/rowSums(tbl),digits=3)))
cat('The overall test data accuracy for predicting a change in temperature state is ',round(sum(tbl*diag(nrow(tbl))/sum(tbl)),digits=3),'\n\n')

selectChange <- test$presentIgbt == test$futureIgbt
tbl <- table(test$futureIgbt[selectChange],test$predicted[selectChange])
print(cbind(as.matrix(tbl),accuracy=round(rowSums(tbl*diag(nrow(tbl)))/rowSums(tbl),digits=3)))
cat('The overall test data accuracy for predicting no change in temperature state is ',round(sum(tbl*diag(nrow(tbl))/sum(tbl)),digits=3),'\n\n')


stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')



