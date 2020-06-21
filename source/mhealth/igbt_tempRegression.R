#######################################################################################
# igbt_tempRegression.R - looks at predicting temperature with regression
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 5, 2016
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filename <- '160505_DR156_Igbt_temp_regression_A_clean.csv'
dt <- read.csv(paste0(dir,filename), nrow=100000)

# shorten the column names
colnames(dt)[2:9] <- substr(colnames(dt)[2:9],30,nchar(colnames(dt)[2:9]))
colnames(dt)[10] <- substr(colnames(dt)[10],30,nchar(colnames(dt)[10]))

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

# copies prior observation's igbt_temp
dt$priorIgbt <- mean(dt$igbt_temp,na.rm=T)
dt$priorIgbt[2:nrow(dt)] <- dt$igbt_temp[1:(nrow(dt)-1)]

# calculates temperature rate of change for prior observation's igbt_temp
dt$priorIgbtSlope <- 0
dt$priorIgbtSlope[3:nrow(dt)] <- (dt$igbt_temp[2:(nrow(dt)-1)] - dt$igbt_temp[1:(nrow(dt)-2)])/
                                  dt$timespan[2:(nrow(dt)-1)]

# calculates watts going into system
dt$watts <- dt$current * dt$dc_bus_voltage

print(data.frame(observations=apply(dt[2:ncol(dt)],2,function(x) length(which(!is.na(x)))),
                 missing=apply(dt[2:ncol(dt)],2,function(x) length(which(is.na(x)))),
                 min=apply(dt[,2:ncol(dt)],2,function(x) round(min(as.numeric(x),na.rm=T),digits=1)),
                 median=apply(dt[,2:ncol(dt)],2,function(x) round(median(as.numeric(x),na.rm=T),digits=1)),
                 max=apply(dt[,2:ncol(dt)],2,function(x) round(max(as.numeric(x),na.rm=T),digits=1)),
                 mean=apply(dt[,2:ncol(dt)],2,function(x) round(mean(as.numeric(x),na.rm=T),digits=1)),
                 sd=apply(dt[,2:ncol(dt)],2,function(x) round(sd(as.numeric(x),na.rm=T),digits=1))))

# lm1 <- lm(igbt_temp~priorIgbt+priorIgbtSlope+priorIgbtSlope2+priorIgbt2,data=dt)

# (regVar <- c('priorIgbt','priorIgbtSlope'))
(regVar <- c('current','dc_bus_voltage','frequency','output_voltage','power','speed','torque','hookload'))
lm1 <- lm(igbt_temp~.,data=dt[,c('igbt_temp',regVar)])
# lm2 <- lm(igbt_temp~priorIgbt+priorIgbtSlope,data=dt)
# lm3 <- lm(igbt_temp~priorIgbt+priorIgbtSlope+watts,data=dt)
# lm4 <- lm(igbt_temp~.,data=dt[,c(2,5,6,8,12,13,14)])

lmp <- lm1
plot(lmp$fitted.values,lmp$fitted.values+lmp$residuals,pch='.',
     main=sprintf('Linear Regression corr=%.3f',cor(lmp$fitted.values+lmp$residuals,lmp$fitted.values)),
     sub=sprintf('formula=%s%s%s',as.character(lmp$call$formula)[2],
                 as.character(lmp$call$formula)[1],
                 as.character(lmp$call$formula)[3]))
prd <- predict(lmp,newdata=dt[,regVar],interval=c('confidence'),level=0.995,type='response')
lines(prd[,1],prd[,2],col='red')
lines(prd[,1],prd[,3],col='red')


# 
# lmp <- lm2
# plot(lmp$fitted.values,lmp$fitted.values+lmp$residuals,pch='.',
#      main=sprintf('Linear Regression corr=%.3f',cor(lmp$fitted.values+lmp$residuals,lmp$fitted.values)),
#      sub=sprintf('formula=%s%s%s',as.character(lmp$call$formula)[2],
#                  as.character(lmp$call$formula)[1],
#                  as.character(lmp$call$formula)[3]))
# 
# lmp <- lm3
# plot(lmp$fitted.values,lmp$fitted.values+lmp$residuals,pch='.',
#      main=sprintf('Linear Regression corr=%.3f',cor(lmp$fitted.values+lmp$residuals,lmp$fitted.values)),
#      sub=sprintf('formula=%s%s%s',as.character(lmp$call$formula)[2],
#                  as.character(lmp$call$formula)[1],
#                  as.character(lmp$call$formula)[3]))
# 
# lmp <- lm4
# plot(lmp$fitted.values,lmp$fitted.values+lmp$residuals,pch='.',
#      main=sprintf('Linear Regression corr=%.3f',cor(lmp$fitted.values+lmp$residuals,lmp$fitted.values)),
#      sub=sprintf('formula=%s%s%s',as.character(lmp$call$formula)[2],
#                  as.character(lmp$call$formula)[1],
#                  as.character(lmp$call$formula)[3]))

# plot(lm3$fitted.values,lm3$residuals,pch='.',col='black')
# points(lm2$fitted.values,lm2$residuals,pch='.',col='red')
# points(lm1$fitted.values,lm1$residuals,pch='.',col='green')
