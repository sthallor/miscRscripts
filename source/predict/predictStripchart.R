###########################################################################
# predictStripchart.R
# strip chart of prediction results
# Ensign Energy Services Inc. retains all rights to this software
# FHS April 28, 2017
##########################################################################
# dt: data frame of prediction data
# title: chart title
# prm.pt: predict training parameters
predictStripchart <- function(dt,title,targetAlarmCutoff,from,to) {

  if (from < 1) from <- 1
  if (from > nrow(dt)) from <- nrow(dt)
  if (to < 1) to <- 1
  if (to > nrow(dt)) to <- nrow(dt)
  if (to < from) to <- from
  
  # display location parameters including double strip at bottom
  numStrips <- 4 # number of strips in strip chart
  interval <- 0.9/(numStrips+1)
  figloc <- t(matrix(rep(0,(4*numStrips)),nrow=4,ncol=numStrips))
  figloc[,1] <- 0 # left position
  figloc[,2] <- 1 # right position
  figloc[,3] <- seq(from=(1-interval-0.02), to=(0.08+interval), by=-interval) # bottom position
  figloc[nrow(figloc),3] <- 0.08
  figloc[,4] <- c(1,figloc[1:(nrow(figloc)-1),3])
  
  # plot margin locations (bottom,left,top,right)
  marloc <- t(matrix(c(0.0,4.2,1.2,0.2,rep(c(0.0,4.2,0.0,0.2),(numStrips-1))),
                     nrow=4,ncol=numStrips))
  
  old.par1 <- par(mfrow=c(1,1))
  old.par2 <- par(mar=marloc[1,])
  old.par3 <- par(fig=figloc[1,])
  
  plot.new()
  
  cex.lab <- 0.7
  dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
  
  for (i in 1:numStrips) {
    par(mar=marloc[i,]) # margin (bottom,left,top,right)    
    par(fig=figloc[i,],new=TRUE) # set display parms
    
    if (i==1) {
      ylabel <- 'block_height'
      yaxt <- NULL
      temp <- dt$block_height
      plot(dt$time[from:to], temp[from:to],type='l',col='grey',xaxt='n',xlab="",
           main=sprintf('%s from=%i to=%i %s',title,from,to,substr(dt$time[from],1,10)),
           ylim=c(min(temp,na.rm=T),max(temp,na.rm=T)),
           ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.lab,yaxt=NULL)
      temp <- dt$block_height
      temp[dt$igbt_temp<targetAlarmCutoff] <- NA
      temp[is.na(dt$igbt_temp)] <- NA
      lines(dt$time[from:to], temp[from:to],col='red')
      points(dt$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
      legend('bottomleft',c('igbt overtemp','igbt OK'),fill=c('red','grey'),cex=0.6)
    } else if (i==2) {
      ylabel <- 'hookload'
      yaxt <- NULL
      temp <- dt$hookload
      plot(dt$time[from:to], temp[from:to],type='l',col='grey',xaxt='n',xlab="",
           ylim=c(min(temp,na.rm=T),max(temp,na.rm=T)),
           ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.lab,yaxt=NULL)
      temp <- dt$hookload
      temp[dt$igbt_temp<targetAlarmCutoff] <- NA
      temp[is.na(dt$igbt_temp)] <- NA
      lines(dt$time[from:to], temp[from:to],col='red')
      points(dt$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
      legend('bottomleft',c('igbt overtemp','igbt OK'),fill=c('red','grey'),cex=0.6)
    } else if (i==3) {
      ylabel <- 'rfGTcut'
      yaxt <- 'n'
      
      temp <- as.numeric(dt$futureIgbtMax>=targetAlarmCutoff)
      plot(dt$time[from:to], temp[from:to], type='l',xaxt='n',xlab="", col='cyan',
           ylab=ylabel,ylim=c(-0.1,1.1),
           cex.lab=cex.lab,cex.axis=cex.lab,yaxt=yaxt)
      axis(side=2,at=seq(0.1,0.9,by=0.4),las=2,cex=cex.lab)
      
      temp <- dt$rfGTcut
      lines(dt$time[from:to], temp[from:to],col='grey')
      
      temp <- dt$rfGTcut
      temp[dt$rfAlarm!=TRUE] <- NA
      lines(dt$time[from:to], temp[from:to],col='red')
      points(dt$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)

      legend('bottomleft',c('igbt alarm','igbt OK','futureIgbtMax'),fill=c('red','grey','cyan'),cex=0.6)
    } else if (i==4) {
      ylabel <- ' igbt_temp'
      yaxt <- NULL
      temp <- dt$igbt_temp
      plot(dt$time[from:to], temp[from:to],type='l',col='grey',xlab="",
           ylim=c((min(dt$igbt_temp,dt$futureIgbtMax,dt$predicted,na.rm=T)*0.99),
                  (max(dt$igbt_temp,dt$futureIgbtMax,dt$predicted,na.rm=T)*1.01)),
           ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.lab,yaxt=yaxt)

      temp <- dt$futureIgbtMax
      lines(dt$time[from:to], temp[from:to],col='cyan')
      
      temp <- dt$predicted
      lines(dt$time[from:to], temp[from:to],col='green')
      
      temp <- dt$igbt_temp
      temp[dt$igbt_temp < targetAlarmCutoff] <- NA
      temp[is.na(dt$igbt_temp)] <- NA
      lines(dt$time[from:to], temp[from:to],col='red')
      points(dt$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
      
      legend('bottomleft',c('igbt_temp','igbt hot','futureIgbtMax','predictedIgbtMax'),
             fill=c('grey','red','cyan','green'),cex=0.6)
    }
  }
  
  par(old.par1) # return to original display parameters
  par(old.par2)
  par(old.par3)
}