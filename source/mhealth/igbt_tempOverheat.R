#######################################################################################
# igbt_tempOverheat.R - looks at predicting whether IGBT temperature will increase
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 15, 2017
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

rowSpan <- 30

dt$hookloadRunMean <- runmean(dt$hookload,k=rowSpan,align='right')
dt$hookloadRunSD <- runsd(dt$hookload,k=rowSpan,align='right')
dt$torqueRunMean <- runmean(dt$torque,k=rowSpan,align='right')
dt$torqueRunSD <- runsd(dt$torque,k=rowSpan,align='right')
dt$igbt_tempRunMean <- runmean(dt$igbt_temp,k=rowSpan,align='right')
dt$igbt_tempRunSD <- runsd(dt$igbt_temp,k=rowSpan,align='right')
dt$kwatts <- dt$current * dt$dc_bus_voltage/1000
dt$kwattsRunMean <- runmean(dt$kwatts,k=rowSpan,align='right')
dt$kwattsRunSD <- runsd(dt$kwatts,k=rowSpan,align='right')
dt$speedRunMean <- runmean(dt$speed,k=rowSpan,align='right')
dt$speedRunSD <- runsd(dt$speed,k=rowSpan,align='right')
dt$output_voltageRunMean <- runmean(dt$output_voltage,k=rowSpan,align='right')
dt$output_voltageRunSD <- runsd(dt$output_voltage,k=rowSpan,align='right')
dt$frequencyRunMean <- runmean(dt$frequency,k=rowSpan,align='right')
dt$frequencyRunSD <- runsd(dt$frequency,k=rowSpan,align='right')
dt$currentRunMean <- runmean(dt$current,k=rowSpan,align='right')
dt$currentRunSD <- runsd(dt$current,k=rowSpan,align='right')
dt$dc_bus_voltageRunMean <- runmean(dt$dc_bus_voltage,k=rowSpan,align='right')
dt$dc_bus_voltageRunSD <- runsd(dt$dc_bus_voltage,k=rowSpan,align='right')
# Cumulative abs delta block height for rowSpan observations to the right
temp <- dt$block_height
temp[1] <- 0
temp[2:nrow(dt)] <- abs(dt$block_height[2:nrow(dt)]-dt$block_height[1:(nrow(dt)-1)])
temp[is.na(temp)] <- 0
temp <- cumsum(temp)
dt$deltaBlockHeightAbs <- 0
# First part calculation
dt$deltaBlockHeightAbs[1:rowSpan] <- temp[1:rowSpan]
# main part calculation
dt$deltaBlockHeightAbs[(rowSpan+1):nrow(dt)] <- temp[(rowSpan+1):nrow(dt)] - 
                                                          temp[1:(nrow(dt)-rowSpan)]


# Sets up future IGBT temperature classifications
rowFuture <- 30  # center of future max temp is 5 minutes 
rowFutureSpan <- 61 # timespan is 5 minutes on either side
dt$futureIgbtMax <- NA
# dt$futureIgbtMax[1:(nrow(dt)-rowFuture)] <- runmax(dt$igbt_temp[(rowFuture+1):nrow(dt)],k=rowFutureSpan) 
dt$futureIgbtMax <- runmax(dt$igbt_temp,k=rowFutureSpan,align='left')



dt$futureIgbt <- NA
dt$futureIgbt[dt$futureIgbtMax<50] <- 'coolf'
dt$futureIgbt[dt$futureIgbtMax>50] <- 'warmf'
dt$futureIgbt[dt$futureIgbtMax>80] <- 'hotf'
dt$futureIgbt[dt$futureIgbtMax>100] <- 'very_hotf'
dt$futureIgbt <- factor(dt$futureIgbt,levels=list(coolf='coolf',warmf='warmf',hotf='hotf',very_hotf='very_hotf'))

cat('\nUp to ',round(rowFutureSpan/6),' minutes in the future IGBT max temperature classifications in dataset\n')
print(table(dt$futureIgbt))

# setup present IGBT temperature classifications
dt$presentIgbt <- NA
dt$presentIgbt[dt$igbt_temp<50] <- 'coolp'
dt$presentIgbt[dt$igbt_temp>50] <- 'warmp'
dt$presentIgbt[dt$igbt_temp>80] <- 'hotp'
dt$presentIgbt[dt$igbt_temp>100] <- 'very_hotp'
dt$presentIgbt <- factor(dt$presentIgbt,levels=list(coolp='coolp',warmp='warmp',hotp='hotp',very_hotp='very_hotp'))

cat('\nPresent actual IGBT temperature classifications in dataset\n')
print(table(dt$presentIgbt))

cat('\n Comparison between present and future IGBT temperature classifications\n')
print(table(dt$presentIgbt,dt$futureIgbt))


###################################################################################
# Sets up random forest training here
sampleSize <- 100000

(regVar <- c('dc_bus_voltage','dc_bus_voltageRunMean','dc_bus_voltageRunSD',
             'output_voltage','output_voltageRunMean','output_voltageRunSD',
             'hookload','hookloadRunMean','hookloadRunSD',
             'torque','torqueRunMean','torqueRunSD',
             'igbt_temp','igbt_tempRunMean','igbt_tempRunSD',
             'kwattsRunMean','kwattsRunSD',
             'speedRunMean','speedRunSD',
             'frequencyRunMean','frequencyRunSD',
             'current','currentRunMean','currentRunSD',
             'deltaBlockHeightAbs'))


set.seed(123)
trainIndex <- 1:sampleSize
# trainIndex <- sample(1:nrow(dt),sampleSize)
train <- dt[trainIndex,]
test <- dt[-trainIndex,]

X.train <- train[complete.cases(train[c(regVar,'futureIgbt')]),(colnames(train) %in% regVar)]
y.train <- train[complete.cases(train[c(regVar,'futureIgbt')]),(colnames(train) %in% 'futureIgbt')]
y.train <- as.factor(y.train)

cat('\n\nRandom forest training with ',nrow(X.train),' observations and ',
    ncol(X.train),' predictors.\n\n')

rf <- randomForest(X.train,y.train,ntree=250,do.trace=50)

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

test$predicted <- NA
test$predicted[complete.cases(test[regVar])] <- predict(rf,test[complete.cases(test[regVar]),regVar])
test$predicted <- factor(test$predicted)
levels(test$predicted) <- c('coolfp','warmfp','hotfp','very_hotfp')
tbl <- table(test$futureIgbt,test$predicted)
print(tbl)
cat('The overall accuracy for predicting the temperature state is ',round(sum(tbl*diag(4)/sum(tbl)),digits=3))

levels(test$presentIgbt) <- levels(test$futureIgbt)
selectChange <- test$presentIgbt != test$futureIgbt
tbl <- table(test$futureIgbt[selectChange],test$predicted[selectChange])
print(tbl)
cat('The overall accuracy for predicting a change in temperature state is ',round(sum(tbl*diag(4)/sum(tbl)),digits=3))

selectChange <- test$presentIgbt == test$futureIgbt
tbl <- table(test$futureIgbt[selectChange],test$predicted[selectChange])
print(tbl)
cat('The overall accuracy for predicting no change in temperature state is ',round(sum(tbl*diag(4)/sum(tbl)),digits=3))

test$coolfVote <- NA
test$warmfVote <- NA
test$hotfVote <- NA
test$very_hotfVote <- NA
test[complete.cases(test[regVar]),c('coolfVote','warmfVote','hotfVote','very_hotfVote')] <- 
  as.data.frame(predict(rf,test[complete.cases(test[regVar]),regVar],type="vote"))


stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')



