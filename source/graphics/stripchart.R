###########################################################################
# stripchart.R
# Ensign Energy Services Inc. retains all rights to this software
# Function to view drill data
# FHS March 18, 2019
##########################################################################
# dd: drill data containing named columns
# from: first display point
# to: last display point
# prm$ddname: names of columns to display
# prm$rig_state: name of rigstate column
# prm$cexv: point size for point display
stripchart <- function(dd, from=1, to=10, prm) {
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
    if (sum(!is.na(dd[[prm$ddname[i]]][from:to]))>0) { # There are values to plot
      par(mar=marloc[i,]) # margin (bottom,left,top,right)    
      par(fig=figloc[i,],new=TRUE) # set display parms
      
      lineColor <- 'grey'
      if (class(dd[[prm$ddname[i]]])!='factor') {
        lineColor <- 'grey'
        if (prm$ddMinYaxis[i] != (-999.25) & prm$ddMinYaxis[i] != prm$ddMaxYaxis[i]) {
          ymin <- prm$ddMinYaxis[i]
        } else {
          ymin <- min(dd[[prm$ddname[i]]][from:to],na.rm=T)
        }
        if (prm$ddMaxYaxis[i] != (-999.25) & prm$ddMinYaxis[i] != prm$ddMaxYaxis[i]) {
          ymax <- prm$ddMaxYaxis[i]
        } else {
          ymax <- max(dd[[prm$ddname[i]]][from:to],na.rm=T)
        }
        yaxt <- NULL
      } else {
        lineColor <- 'white'
        ymin <- length(levels(dd[[prm$ddname[i]]][from:to])) + 1
        ymax <- 0
        yaxt <- 'n'
      }
      # sets up ylabel according to max length allowed
      if (prm$doublebottomstripsize & i==length(prm$ddname)) {
        ylabel <- if(nchar(prm$ddname[i])<(2*prm$maxYlabelLength+1)) prm$ddname[i] else 
          substr(prm$ddname[i],(nchar(prm$ddname[i])-2*prm$maxYlabelLength+1),nchar(prm$ddname[i]))
      } else {
        ylabel <- if(nchar(prm$ddname[i])<(prm$maxYlabelLength+1)) prm$ddname[i] else 
          substr(prm$ddname[i],(nchar(prm$ddname[i])-prm$maxYlabelLength+1),nchar(prm$ddname[i]))
      }
      # if (substr(prm$ddname[i],1,4)=='EDR_') ylabel <- substr(prm$ddname[i],5,nchar(prm$ddname[i]))
      
      if (i==1) {
        plot(dd$time[from:to], dd[[prm$ddname[i]]][from:to], type='l',xaxt='n',xlab="", col=lineColor,
             ylab=ylabel,ylim=if (prm$ddReverseYaxis[i]==TRUE) c(ymax,ymin) else c(ymin,ymax),
             main=if (is.null(dd$Rig)) { # Don't include rig # in title if non-existent
                 sprintf("%s %s  Pts: %i to % i",
                         prm$title,
                         substr(as.character(dd$time[from]),1,10),from,to)
               } else {
                 sprintf("%s Rig %s %s  Pts: %i to % i",
                         prm$title,as.character(dd$Rig[from]),
                         substr(as.character(dd$time[from]),1,10),from,to)
               },
             cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
      } else if (i<length(prm$ddname)) {
          plot(dd$time[from:to], dd[[prm$ddname[i]]][from:to], type='l',xaxt='n',xlab="",col=lineColor,
               ylab=ylabel,ylim=if (prm$ddReverseYaxis[i]==TRUE) c(ymax,ymin) else c(ymin,ymax),
               cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
      } else {
        plot(dd$time[from:to], dd[[prm$ddname[i]]][from:to], type='l',xlab="",col=lineColor,
             ylab=ylabel,ylim=if (prm$ddReverseYaxis[i]==TRUE) c(ymax,ymin) else c(ymin,ymax),
             cex.lab=cex.lab,cex.axis=cex.axis,yaxt=yaxt)
      }
      
      if (is.null(dd[[prm$rig_state]])) {
        points(dd$time[from:to],dd[[prm$ddname[i]]][from:to],pch=16,cex=prm$cexv, col='black')
      } else { # a rig_state is selected so display
        if (class(dd[[prm$ddname[i]]])!='factor') {
          # numeric display (not factor), loops through the prm$dv display parameters
          for (l in 1:nrow(prm$dv)) {
            # selects observations for display with current display parameters (rig state factor)
            ip <- which(dd[[prm$rig_state]][from:to]==prm$dv$rig_state[l])+from-1
            # plot selected observations
            points(dd$time[ip],dd[[prm$ddname[i]]][ip],pch=prm$dv$pch[l],cex=prm$cexv*prm$dv$cex[l], 
                   col=prm$dv$color[l])
          }
          # If it is the first strip, display color/symbol legend
          if (i==1) {
            # Add record count for strip for each rig state classification, FHS March 18, 2019
            prm$dv$rig_state_label <- as.character(prm$dv$rig_state)
            prm$dv$rig_state_count <- 0
            prm$dv$rig_state_rfvote <- 0
            for (l in 1:nrow(prm$dv)) {
              prm$dv$rig_state_count[l] <- length(which(dd[[prm$rig_state]][from:to]==prm$dv$rig_state[l]))
              if (prm$dv$rig_state_count[l]>0) {
                prm$dv$rig_state_label[l] <- paste(as.character(prm$dv$rig_state[l]),' n=',
                                                   as.character(prm$dv$rig_state_count[l]),
                                                   sep='')
                if (!is.null(dd$rfvote)) {
                  temp <- dd$rfvote[from:to]
                  prm$dv$rig_state_rfvote <- mean(dd$rfvote[from:to][which(dd[[prm$rig_state]][from:to]==prm$dv$rig_state[l])],na.rm=T)
                  prm$dv$rig_state_label[l] <- paste(prm$dv$rig_state_label[l],' rfv=',
                                                  as.character(round(prm$dv$rig_state_rfvote[l],digits=3)),
                                                  sep='')
                }
              }
            }
            if (sum(prm$dv$rig_state_count)>0) {
              legend('topleft',prm$dv$rig_state_label[prm$dv$rig_state_count>0],
                     pch=prm$dv$pch[prm$dv$rig_state_count>0],
                     col=prm$dv$color[prm$dv$rig_state_count>0],
                     cex=prm$cexv)
            }
          }
        } else { # factor based strip chart
          # check if there are color/symbol parameters for display
          if (sum(prm$dfv$ddname==prm$ddname[i])==0) { # No color/symbol parameters
            for (l in 1:length(levels(dd[[prm$ddname[i]]]))) {
              ip <- which(dd[[prm$ddname[i]]][from:to]==levels(dd[[prm$ddname[i]]])[l])+from-1
              points(dd$time[ip],dd[[prm$ddname[i]]][ip],pch=16,cex=prm$cexv,col=l)
            }
          } else { # There are color/symbol parameters
            for (l in 1:length(levels(dd[[prm$ddname[i]]]))) {
              index <- which(prm$dfv$ddname==prm$ddname[i] & prm$dfv$factor==levels(dd[[prm$ddname[i]]])[l])
              # cat('l=',l,' index=',index,' levels(dd[[prm$ddname[i]]])[l]=',levels(dd[[prm$ddname[i]]])[l],
              #     ' prm$dfv$color[index]=',prm$dfv$color[index],'\n')
              ip <- which(dd[[prm$ddname[i]]][from:to]==levels(dd[[prm$ddname[i]]])[l])+from-1
              if (length(index)>0) {
                points(dd$time[ip],dd[[prm$ddname[i]]][ip],pch=16,cex=prm$cexv,col=prm$dfv$color[index])
              }
            }
          }
        }
        
      }
    }
    
    # Factor level strip chart annotation on the left side
    if (class(dd[[prm$ddname[i]]])=='factor') {
      # covers area to be anotated with white rectangle 
#       rect(dd$time[from],0.1,(dd$time[from]+(dd$time[to]-dd$time[from])/10),
#            (length(levels(dd[[prm$ddname[i]]]))+0.9),
#            col='white',border=FALSE)
      # if there are no color parameters, uses defaults
      if (sum(prm$dfv$ddname==prm$ddname[i])==0) {
        for (l in 1:length(levels(dd[[prm$ddname[i]]]))) {
          text(dd$time[from],l,levels(dd[[prm$ddname[i]]])[l],
               pos=4,cex=(cex.axis*1.0),col=l)
        }
      } else { # there are color parameters in prm$dfv for prm$ddname[i]
        for (l in 1:length(levels(dd[[prm$ddname[i]]]))) {
          index <- which(prm$dfv$ddname==prm$ddname[i] & prm$dfv$factor==levels(dd[[prm$ddname[i]]])[l])
          # cat('l=',l,' index=',index,' levels(dd[[prm$ddname[i]]])[l]=',levels(dd[[prm$ddname[i]]])[l],
          #     ' prm$dfv$color[index]=',prm$dfv$color[index],'\n')
          if (length(index)>0) {
            text(dd$time[from],l,levels(dd[[prm$ddname[i]]])[l],
                 pos=4,cex=cex.axis,col=prm$dfv$color[index])
          }
        }
        
      }
    }
    
  }   
  par(old.par1) # return to original display parameters
  par(old.par2)
  par(old.par3)
}
