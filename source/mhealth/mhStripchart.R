###########################################################################
# mhStripchart1.R
# customized function for viewing machine health forescasts
# Ensign Energy Services Inc. retains all rights to this software
# FHS Mar 2, 2017
##########################################################################
# dd: drill data containing named columns
# from: first display point
# to: last display point
# prm$ddname: names of columns to display
# prm$rig_state: name of rigstate column
# prm$cexv: point size for point display
mhStripchart1 <- function(dd, from=1, to=10, prm) {
  
  if (from < 1) from <- 1
  if (from > nrow(dd)) from <- nrow(dd)
  if (to < 1) to <- 1
  if (to > nrow(dd)) to <- nrow(dd)
  if (to < from) to <- from
  
  # Fix some dataframe classes
  prm$dv$rig_state <- as.character(prm$dv$rig_state)
  prm$dv$color <- as.character(prm$dv$color)
  prm$dfv$color <- as.character(prm$dfv$color)
  
  # display location parms (left,right,bottom,top)
  if (prm$doublebottomstripsize) {
    interval <- 0.9/(length(prm$ddname)+1)
    figloc <- t(matrix(rep(0,(4*length(prm$ddname))),nrow=4,ncol=length(prm$ddname)))
    figloc[,1] <- 0 # left position
    figloc[,2] <- 1 # right position
    figloc[,3] <- seq(from=(1-interval-0.02), to=(0.08+interval), by=-interval) # bottom position
    figloc[nrow(figloc),3] <- 0.08
    figloc[,4] <- c(1,figloc[1:(nrow(figloc)-1),3])
    
  } else {
    interval <- 0.9/length(prm$ddname)
    figloc <- t(matrix(rep(0,(4*length(prm$ddname))),nrow=4,ncol=length(prm$ddname)))
    figloc[,1] <- 0 # left position
    figloc[,2] <- 1 # right position
    figloc[,3] <- seq(from=(1-interval-0.02), to=0.08, by=-interval) # bottom position
    figloc[,4] <- c(1,seq(from=(1-interval-0.02), to=(0.08+interval), by=-interval))
  }
  
  # plot margin locations (bottom,left,top,right)
  marloc <- t(matrix(c(0.0,4.2,1.2,0.2,rep(c(0.0,4.2,0.0,0.2),(length(prm$ddname)-1))),
                     nrow=4,ncol=length(prm$ddname)))
  
  old.par1 <- par(mfrow=c(1,1))
  old.par2 <- par(mar=marloc[1,])
  old.par3 <- par(fig=figloc[1,])
  
  plot.new()

  cex.lab <- prm$cexLabelFactor/length(prm$ddname)
  if (cex.lab > 1) cex.lab=1
  cex.axis <- cex.lab
  for (i in 1:length(prm$ddname)) {
      par(mar=marloc[i,]) # margin (bottom,left,top,right)    
      par(fig=figloc[i,],new=TRUE) # set display parms
      
      if (prm$doublebottomstripsize & i==length(prm$ddname)) {
        ylabel <- if(nchar(prm$ddname[i])<(2*prm$maxYlabelLength+1)) prm$ddname[i] else 
          substr(prm$ddname[i],(nchar(prm$ddname[i])-2*prm$maxYlabelLength+1),nchar(prm$ddname[i]))
      } else {
        ylabel <- if(nchar(prm$ddname[i])<(prm$maxYlabelLength+1)) prm$ddname[i] else 
          substr(prm$ddname[i],(nchar(prm$ddname[i])-prm$maxYlabelLength+1),nchar(prm$ddname[i]))
      }
      
      if (i==1) {
        yaxt <- NULL
        temp <- dd$block_height
        plot(dd$time[from:to], temp[from:to],type='l',col='grey',xaxt='n',xlab="",
             main=sprintf('%s from=%i to=%i %s',prm$title,from,to,substr(dd$time[from],1,10)),
             ylim=c(min(temp,na.rm=T),max(temp,na.rm=T)),
             ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
        temp <- dd$block_height
        temp[dd$igbt_temp<100] <- NA
        lines(dd$time[from:to], temp[from:to],col='red')
        points(dd$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
        legend('bottomleft',c('igbt overtemp','igbt OK'),fill=c('red','grey'),cex=0.6)
      } else if (i==2) {
        yaxt <- NULL
        temp <- dd$hookload
        plot(dd$time[from:to], temp[from:to],type='l',col='grey',xaxt='n',xlab="",
             ylim=c(min(temp,na.rm=T),max(temp,na.rm=T)),
             ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
        temp <- dd$hookload
        temp[dd$igbt_temp<100] <- NA
        lines(dd$time[from:to], temp[from:to],col='red')
        points(dd$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
        legend('bottomleft',c('igbt overtemp','igbt OK'),fill=c('red','grey'),cex=0.6)
      } else if (i==3) {
        yaxt <- 'n'
        temp <- dd$rfGT100
        plot(dd$time[from:to], temp[from:to], type='l',xaxt='n',xlab="", col='grey',
             ylab=ylabel,ylim=c(-0.1,1.1),
             cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
        axis(side=2,at=seq(0.1,0.9,by=0.4),las=2)
        # temp <- dd$rfGT100
        # temp[temp<rfGT100CutoffWarning] <- NA
        # lines(dd$time[from:to], temp[from:to],col='yellow2')
        temp <- dd$rfGT100
        temp[temp<rfGT100CutoffAlarm] <- NA
        lines(dd$time[from:to], temp[from:to],col='red')
        temp <- as.numeric(dd$futureIgbtMax >= 100)*1.1 - 0.05
        lines(dd$time[from:to], temp[from:to],col='cyan')
        # legend('bottomleft',c('igbt alarm','igbt warning','igbt OK','futureIgbtMax'),fill=c('red','yellow2','grey','cyan'),cex=0.6)
        legend('bottomleft',c('igbt alarm','igbt OK','futureIgbtMax'),fill=c('red','grey','cyan'),cex=0.6)
      } else if (i==4) {
        yaxt <- NULL
        temp <- dd$igbt_temp
        plot(dd$time[from:to], temp[from:to],type='l',col='grey',xlab="",
             ylim=c(min(test$igbt_temp,na.rm=T),max(test$igbt_temp,na.rm=T)),
             ylab=ylabel,cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
        
        temp <- dd$igbt_temp
        temp[dd$igbt_temp < 100] <- NA
        lines(dd$time[from:to], temp[from:to],col='red')
        points(dd$time[from:to], temp[from:to],col='red',pch=16,cex=0.25)
        
        temp <- dd$futureIgbtMax
        lines(dd$time[from:to], temp[from:to],col='cyan')
        
        temp <- dd$predicted
        lines(dd$time[from:to], temp[from:to],col='green')
        
        legend('bottomleft',c('igbt_temp','igbt hot','futureIgbtMax','predictedIgbtMax'),
               fill=c('grey','red','cyan','green'),cex=0.6)
      } 
  }   
  par(old.par1) # return to original display parameters
  par(old.par2)
  par(old.par3)
}
