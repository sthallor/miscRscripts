###########################################################################
# hexp1.R
# Ensign Energy Services Inc. retains all rights to this software
# Historian data exploration
# FHS May 13, 2016
##########################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filename <- '160503_DR156_abb_dw_A_clean.csv'
dt <- read.csv(paste0(dir,filename), nrow=-1)
# shorten the column names
colnames(dt)[2:ncol(dt)] <- substr(colnames(dt)[2:ncol(dt)],30,nchar(colnames(dt)[2:ncol(dt)]))
for (i in 2:ncol(dt)) dt[,i] <- as.numeric(dt[,i])

print(data.frame(observations=apply(dt[2:ncol(dt)],2,function(x) length(which(!is.na(x)))),
                 missing=apply(dt[2:ncol(dt)],2,function(x) length(which(is.na(x)))),
                 min=apply(dt[,2:ncol(dt)],2,function(x) min(as.numeric(x),na.rm=T)),
                 median=apply(dt[,2:ncol(dt)],2,function(x) median(as.numeric(x),na.rm=T)),
                 max=apply(dt[,2:ncol(dt)],2,function(x) max(as.numeric(x),na.rm=T)),
                 mean=apply(dt[,2:ncol(dt)],2,function(x) round(mean(as.numeric(x),na.rm=T),digits=1)),
                 sd=apply(dt[,2:ncol(dt)],2,function(x) round(sd(as.numeric(x),na.rm=T),digits=1))))

# Keep only non-redundant (not very highly correlated) variables
# data.attributes <- c('igbt_temp','pp_temp1','rpm_ref',
#                      'output_voltage','ctrl_board_temp',
#                      'dc_bus_voltage','fan_on_time',
#                      'current','torque','power',
#                      'frequency','ro1')
# dateTime <- dt[,1]
# dt <- dt[,(colnames(dt) %in% data.attributes)]
title <- sprintf('DR156 abb_dw %s to %s',substr(dt$time[1],1,10),
                 substr(dt$time[length(dt$time)],1,10))
# Look at histograms
for (i in 2:ncol(dt)) {
  hist(dt[,i],breaks=100,main=title,xlab=colnames(dt)[i])
}