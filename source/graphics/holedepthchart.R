###########################################################################
# holedepthchart.R
# Function to view hole and bit depth data colored coded by rig state
# Ensign Energy Services Inc. retains all rights to this software
# FHS Apr 22, 2016
##########################################################################
# bitDepth: vector of bit depth observation values for y-axis
# holeDepth: vector of hole depth observation values for y-axis
# rigState: vector of rig state observation values for color code
# time: vector of timestamp observation values for x-axis
# dv: data frame with $rig_state and $color vectors for color coding
# title: string with title for top of plot
holedepthchart <- function(bitDepth,holeDepth,lengthUnits,rigState,time,dv,title) {

  rigState <- as.character(rigState)
  dv$color <- as.character(dv$color)
  dv$rig_state <- as.character(dv$rig_state)

  plot(time,holeDepth,pch='.',main=sprintf('%s Bit/Hole Depth',title),
       ylab=paste0('Hole/Bit Depth ',lengthUnits), ylim=c(max(holeDepth,na.rm=TRUE),0),col='grey',
       xlab='time')
  
  dv$count <- 0
  for (i in 1:nrow(dv)) {
    selected <- which(rigState==dv$rig_state[i])
    points(time[selected],bitDepth[selected],pch='.',col=dv$color[i])
    dv$count[i] <- length(selected)
  }
  legend('bottomleft',paste0(dv$rig_state,' obs=',as.character(dv$count)),fill=dv$color,cex=1)
}