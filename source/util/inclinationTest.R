# Exploratory testing with inclination data from many files
# FHS Apr 7, 2016

Options(width=132)
library(caTools)
dir <- '/Users/Fred.Seymour/EDR_Data/160406_BatchTest/'
fileSuffix <- '_input_EDR_clean.csv'
fileName <- c('48512','48702','48808','48834','48858',
              '48865','48880','48908','48931','48936',
              '48945','48948','48950','48953','106447',
              '106449','209462','239404','262647','345634',
              '345640','345643','345645','347024','347066',
              '347553','347554','347556','347557','347558')

id <- data.frame(Well=NULL,Rig=NULL,NObs=NULL,NCol=NULL,HoleDepth=NULL,Present=NULL,
                 Min=NULL,Max=NULL,NStart=NULL,NLimits=NULL,NDrilling=NULL,
                 NRunLength=NULL,NSlope=NULL)

# for (i in 1:length(fileName)) {
for(i in 11:11) {
  fileLoad <- paste0(fileName[i],fileSuffix)
  dt <- read.csv(paste0(dir,fileLoad),nrows=-1)
  cat('\nLoaded ',fileName[i],' with ',nrow(dt),' rows & ',ncol(dt),' cols ')
  
  idCurrent <- data.frame(Well=fileName[i],
                          Rig=dt$Rig[1],
                          NObs=nrow(dt),
                          NCol=ncol(dt),
                          HoleDepth=round(max(dt$EDR_HoleDepth,na.rm=TRUE)),
                          Present=!is.null(dt$EDR_Inclination),
                          Min=NA,
                          Max=NA,
                          NStart=NA,
                          NLimits=NA,
                          NDrilling=NA,
                          NRunLength=NA,
                          NSlope=NA)
  
  if (!is.null(dt$EDR_Inclination)) {
    idCurrent$Min <- if (sum(!is.na(dt$EDR_Inclination))>0) 
      min(dt$EDR_Inclination,na.rm=TRUE) else NA
    idCurrent$Max <- if (sum(!is.na(dt$EDR_Inclination))>0) 
      max(dt$EDR_Inclination,na.rm=TRUE) else NA
    idCurrent$NStart <- sum(!is.na(dt$EDR_Inclination))
    
    
    # step 1, discard any inclination values less than zero or greater than 110
    dt$inclinationSelect <- dt$EDR_Inclination
    dt$inclinationSelect[dt$inclinationSelect<0] <- NA
    dt$inclinationSelect[dt$inclinationSelect>110] <- NA
    idCurrent$NLimits <- sum(!is.na(dt$inclinationSelect))
    
    # step 2, only consider inclination values measured during RDrilling or SDrilling
    dt$inclinationSelect[dt$rig_state!='RDrilling' & dt$rig_state!='SDrilling'] <- NA
    idCurrent$NDrilling <- sum(!is.na(dt$inclinationSelect))
    
    # step 3, discard constant value runs of duration greater than maxLength
    maxLength <-30
    dt$inclinationSelect[is.na(dt$inclinationSelect)] <- (-999)
    ra <- data.frame(values=rle(as.vector(as.character(dt$inclinationSelect)))$values,
                     lengths=rle(as.vector(as.character(dt$inclinationSelect)))$lengths)
    ra$from <- 0
    ra$to <- 0
    ra$to <- cumsum(ra$lengths)
    ra$from <- ra$to - ra$lengths + 1
    for (j in 1:nrow(ra)) {
      if (ra$lengths[j]>maxLength) dt$inclinationSelect[ra$from[j]:ra$to[j]] <- NA
    }
    dt$inclinationSelect[dt$inclinationSelect==(-999)] <- NA
    idCurrent$NRunLength <- sum(!is.na(dt$inclinationSelect))
    
    # step 4, discard values where smoothed abs(smoothDeltaIncl/smoothDeltaHD) > threshold
    threshold <- 0.1
    smoothCount <- 1800
    epsilon <- 1e-5
    dt$deltaIncl <- 0
    dt$deltaIncl[2:nrow(dt)] <- dt$inclinationSelect[2:nrow(dt)] - dt$inclinationSelect[1:(nrow(dt)-1)]
    dt$deltaIncl <- runmean(dt$deltaIncl,smoothCount)
    dt$deltaHD <- 0
    dt$deltaHD[2:nrow(dt)] <- dt$EDR_HoleDepth[2:nrow(dt)] - dt$EDR_HoleDepth[1:(nrow(dt)-1)]
    dt$deltaHD <- runmean(dt$deltaHD,smoothCount)
    dt$deltaHD[dt$deltaHD<epsilon] <- epsilon
    dt$inclDivHD <- dt$deltaIncl/dt$deltaHD
    dt$inclDivHD[is.na(dt$inclinationSelect)] <- NA
    dt$inclinationSelect[abs(dt$inclDivHD)>threshold] <- NA
    dt$inclDivHD[abs(dt$inclDivHD)>threshold] <- NA
    idCurrent$NSlope <- sum(!is.na(dt$inclinationSelect))
    
    # Now we are ready to determine the drill stage based on inclination
    dt$drillStage <- 'Unknown'
    dt$drillStage[!is.na(dt$inclinationSelect)] <- "Vertical"
    dt$drillStage[dt$inclinationSelect>10] <- "BuildingAngle"
    dt$drillStage[dt$inclinationSelect>70] <- "Lateral"
    dt$drillStage <- as.character(dt$drillStage)
    
    ra <- data.frame(values=rle(as.vector(as.character(dt$drillStage)))$values,
                     lengths=rle(as.vector(as.character(dt$drillStage)))$lengths)
    ra$from <- 0
    ra$to <- 0
    ra$to <- cumsum(ra$lengths)
    ra$from <- ra$to - ra$lengths + 1 
    ra$values <- as.character(ra$values)
    
    if (nrow(ra)>1) {
      if (ra$values[nrow(ra)]=='Unknown') {
        ra$values[nrow(ra)] <- ra$values[(nrow(ra)-1)]
        dt$drillStage[ra$from[nrow(ra)]:ra$to[nrow(ra)]] <- ra$values[nrow(ra)]
      }
      for (j in (nrow(ra)-1):1) {
        if (ra$values[j]=='Unknown') {
          ra$values[j] <- ra$values[j+1]
        } else if (ra$values[j]=='Lateral') {
          if (ra$values[j+1]!='Lateral') ra$values[j] <- ra$values[j+1]
        } else if (ra$values[j]=='BuildingAngle') {
          if (ra$values[j+1]=='Vertical') ra$values[j] <- ra$values[j+1]
        }
        dt$drillStage[ra$from[j]:ra$to[j]] <- ra$values[j]
      }
    }

    # Create plot
    dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
    minIncl <- -10
    maxIncl <- 130
    labelLocs <- seq(from=0,to=120,by=40)
    
    plot(dt$time,
         dt$EDR_Inclination,
         type='p',pch='.',cex=0.4,col='yellow2',yaxt='n',main=fileLoad,xlab='',ylab='', ylim=c(minIncl,maxIncl))
    abline(h=10,col='grey')
    abline(h=70,col='grey')
    
    axis(side=2,line=0, at=labelLocs,col='blue',col.ticks='blue',col.axis='blue')
    
    # scale and plot hole depth
    minHD <- min(dt$EDR_HoleDepth,na.rm=TRUE)
    maxHD <- max(dt$EDR_HoleDepth,na.rm=TRUE)
    dt$scaleHD <- ((maxHD-dt$EDR_HoleDepth)/(maxHD-minHD))*(maxIncl-minIncl)+minIncl
    labelHD <- round(((maxIncl-labelLocs)/(maxIncl-minIncl))*(maxHD-minHD)+minHD)
    
    points(dt$time[dt$drillStage=='Vertical'],dt$scaleHD[dt$drillStage=='Vertical'],cex=0.8,pch=20,col='green')
    points(dt$time[dt$drillStage=='BuildingAngle'],dt$scaleHD[dt$drillStage=='BuildingAngle'],cex=0.8,pch=20,col='magenta')
    points(dt$time[dt$drillStage=='Lateral'],dt$scaleHD[dt$drillStage=='Lateral'],cex=0.8,pch=20,col='cyan')
    axis(side=2,line=2,at=labelLocs,labels=as.character(labelHD),
         col='magenta',col.ticks='magenta',col.axis='magenta')

    minDiv <- min(dt$inclDivHD,na.rm=TRUE)
    maxDiv <- max(dt$inclDivHD,na.rm=TRUE)
    dt$scaleInclDivHD <- ((dt$inclDivHD-minDiv)/(maxDiv-minDiv))*(maxIncl-minIncl)+minIncl
    labelDiv <- round(((labelLocs-minIncl)/(maxIncl-minIncl))*(maxDiv-minDiv)+minDiv,digits=5)
    
#     points(dt$time,dt$scaleInclDivHD,pch='.',col='red')
#     axis(side=4,line=0,at=labelLocs,labels=as.character(labelDiv),
#          col='red',col.ticks='red',col.axis='red')
    
    dt$inclinationRunMean <- runmean(dt$inclinationSelect,smoothCount)
    dt$inclinationRunMean[dt$rig_state!='RDrilling' & dt$rig_state!='SDrilling'] <- NA
    points(dt$time,dt$inclinationRunMean,pch=20,cex=0.8,col='blue')

    
    legend('topright',c('HoleDepth Vertical',
                    'HoleDepth BuildingAngle',
                    'HoleDepth Lateral',
                    'EDR_Inclination All (deg)',
                    'EDR_Inclination Cleaned'),
           col=c('green','magenta','cyan','yellow2','blue'),
           text.col=c('green','magenta','cyan','yellow2','blue'),cex=0.7)
    
#     plot(dt$EDR_HoleDepth,dt$inclinationSelect,pch=20, ylim=c(-10,120))
#     abline(h=10,col='grey')
#     abline(h=70,col='grey')
  }
  id <- rbind(id,idCurrent)
}
cat('\n\nSummary of EDR_Inclination results\n')
print(id)
