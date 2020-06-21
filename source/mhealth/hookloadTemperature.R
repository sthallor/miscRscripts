#######################################################################################
# hookloadTemperature.R - looking at historian DR156 hookload and temp data
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 4, 2016
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filename <- '160504_DR156_dw_hookload_vs_temp_A_clean.csv'
dt <- read.csv(paste0(dir,filename), nrow=-1)

# shorten the column names
colnames(dt)[2] <- substr(colnames(dt)[2],30,nchar(colnames(dt)[2]))
colnames(dt)[3:7] <- substr(colnames(dt)[3:7],30,nchar(colnames(dt)[3:7]))
colnames(dt)[8:9] <- substr(colnames(dt)[8:9],26,nchar(colnames(dt)[8:9]))

dt$time <- as.character(dt$time)
for (i in 2:ncol(dt)) dt[,i] <- as.numeric(dt[,i])

print(data.frame(observations=apply(dt[2:ncol(dt)],2,function(x) length(which(!is.na(x)))),
                 missing=apply(dt[2:ncol(dt)],2,function(x) length(which(is.na(x)))),
                 min=apply(dt[,2:ncol(dt)],2,function(x) round(min(as.numeric(x),na.rm=T),digits=1)),
                 median=apply(dt[,2:ncol(dt)],2,function(x) round(median(as.numeric(x),na.rm=T),digits=1)),
                 max=apply(dt[,2:ncol(dt)],2,function(x) round(max(as.numeric(x),na.rm=T),digits=1)),
                 mean=apply(dt[,2:ncol(dt)],2,function(x) round(mean(as.numeric(x),na.rm=T),digits=1)),
                 sd=apply(dt[,2:ncol(dt)],2,function(x) round(sd(as.numeric(x),na.rm=T),digits=1))))

# Look at histograms
title <- sprintf('DR156 %s to %s',substr(dt$time[1],1,10),
                 substr(dt$time[nrow(dt)],1,10))
for (i in 2:ncol(dt)) {
  hist(dt[,i],breaks=100,main=title,xlab=colnames(dt)[i])
}

# look at pair plots with correlations
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
count <- 10000
sampleSelect <- sample(1:nrow(dt),count)
dt1 <- dt[sampleSelect,]
pairs(dt1[,c(2:ncol(dt))], pch='.', gap=0.2,upper.panel=panel.cor, 
      main=sprintf('%s %i samples cor in upperT',title,count))

##############################################################################
# Looking for effect of hookload * blockspeed on temperature over time
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
dt$timespan <- 0
dt$timespan[1] <- as.numeric(difftime(dt$time[2],dt$time[1],units='secs'))
dt$timespan[2:(nrow(dt)-1)] <- as.numeric(difftime(dt$time[3:nrow(dt)],
                                                   dt$time[1:(nrow(dt)-2)],units='secs'))/2
dt$timespan[nrow(dt)] <- as.numeric(difftime(dt$time[nrow(dt)],
                                             dt$time[(nrow(dt)-1)],units='secs'))

# A little data cleanup
# dt$hookload[dt$hookload>15000] <- 15000
# dt$blockspeed_true[dt$blockspeed_true<(-30)] <- (-30)
# dt$blockspeed_true[dt$blockspeed_true>30] <- 30

dt$hookEnergy <- abs(dt$torque*dt$blockspeed_true*dt$timespan)
# dt$hookEnergy[dt$hookEnergy < 1] <- 1
# dt$hookEnergy <- log(dt$hookEnergy)

hist(dt$hookEnergy,breaks=100,main=title,xlab='hook "Energy" (hookload * blockspeed)')

# Runing average for hookEnergy
library(caTools)

# smoothCount <- c(6,15,30,60,90,120,180,270,360,450,540,630,720)
smoothCount <- c(1,3,6,9,12,18,24,30,36,48,60,75,90,120,150,180)

correlations <- data.frame(smoothCount=smoothCount,
                           igbt_tempCorr=rep(0,length(smoothCount)),
                           ctrl_board_tempCorr=rep(0,length(smoothCount)),
                           pp_temp1Corr=rep(0,length(smoothCount)))
for (i in 1:length(smoothCount)) {
  dt$runMean <- runmean(dt$current,smoothCount[i],align='left')
  select <- !is.na(dt$blockspeed_true)
  correlations$igbt_tempCorr[i] <- cor(dt$runMean[select],dt$igbt_temp[select],use="complete.obs")
  correlations$ctrl_board_tempCorr[i] <- cor(dt$runMean[select],dt$ctrl_board_temp[select],use="complete.obs")
  correlations$pp_temp1Corr[i] <- cor(dt$runMean[select],dt$pp_temp1[select],use="complete.obs")
}

print(correlations)
plot(correlations$smoothCount,correlations$igbt_tempCorr,col='red',type='l',main=title,ylim=c(0,1),
     ylab='Correlation with hookEnergy', xlab='HookEnergy smooth count')
lines(correlations$smoothCount,correlations$ctrl_board_tempCorr,col='green')
lines(correlations$smoothCount,correlations$pp_temp1Corr,col='blue')
legend('topleft',c('igbt_temp','ctrl_board_temp','pp_temp1'),fill=c('red','green','blue'))




