##########################################################################
# mainStripchart.R - create strip charts from DR data
# Ensign Energy Services Inc. retains all rights to this software
# FHS Nov 30, 2017
##########################################################################
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/graphics/get.prm.sc.R"))
source(paste0(sourceDir,"source/graphics/stripchart.R"))
source(paste0(sourceDir,"source/graphics/holedepthchart.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,"parms/dev/util/stripchart.prm")
  cat("\nNo command line argument ... using default parameters filename\n",prmFilename,"\n\n")
}

#########################################################################
# First, get the strip chart parameters
prm.sc <- get.prm.sc(prmFilename)

if (!prm.sc$plotFilename=="") {
  cat("\nSending stripchart output to pdf file:\n",prm.sc$plotFilename,"\n")
  if (file.exists(prm.sc$plotFilename)) {
    # Removes file because of apparent bug in pdf reader that does not allow
    # pdf file to be overwritten after it has been openned even if it has been closed
    # FHS Nov 30, 2017
    file.remove(prm.sc$plotFilename)
    Sys.sleep(1)
  }

  pdf(file=prm.sc$plotFilename)
}

#########################################################################
# Second, read the DR file
dt <- read.csv(prm.sc$filename,nrows=prm.sc$to)
cat('Strip charts from file ',prm.sc$filename)

if (!('time' %in% colnames(dt))) {
  cat('\n\nFATAL ERROR in stripchart... need to have "time" column..')
  cat('\nHave you run dclean on the file?\n\n')
  stop('stripchart ERROR ... no "time" column')
}
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")

##########################################################################
# Third, create the summary DR hole depth charts

if ('EDR_BitDepth' %in% colnames(dt) & 
    'EDR_HoleDepth' %in% colnames(dt) &
    prm.sc$rig_state %in% colnames(dt)) {
  holedepthchart(bitDepth=dt$EDR_BitDepth,
                 holeDepth=dt$EDR_HoleDepth,
                 lengthUnits=if('length_uom' %in% colnames(dt)) dt$length_uom[1] else '',
                 rigState=dt[[prm.sc$rig_state]],
                 time=dt$time,
                 dv=prm.sc$dv,
                 title=paste0('Rig ',dt$Rig[1],' Rig State ',prm.sc$title))
}

if ('EDR_BitDepth' %in% colnames(dt) & 
    'EDR_HoleDepth' %in% colnames(dt) &
    'activity' %in% colnames(dt)) {
  holedepthchart(bitDepth=dt$EDR_BitDepth,
                 holeDepth=dt$EDR_HoleDepth,
                 lengthUnits=if('length_uom' %in% colnames(dt)) dt$length_uom[1] else '',
                 rigState=dt$activity,
                 time=dt$time,
                 dv=data.frame(rig_state=c('TrippingIn','TrippingOut','Drilling','NotDrilling'),
                               color=c('cyan','yellow2','green','red')),
                 title=paste0('Rig ',dt$Rig[1],' Activity ',prm.sc$title))
}

#     # Only need if inclinationState is used.  FHS 3/15/2016    
#     if ('inclinationState' %in% colnames(dt)) {
#       points(dt$time[dt$inclinationState=="Vertical"],rep(-50,sum(dt$inclinationState=="Vertical")),pch='.',col='grey')
#       points(dt$time[dt$inclinationState=="BuildingAngle"],rep(-50,sum(dt$inclinationState=="BuildingAngle")),pch='.',col='magenta')
#       points(dt$time[dt$inclinationState=="Lateral"],rep(-50,sum(dt$inclinationState=="Lateral")),pch='.',col='blue')
#       legend('bottomleft',c('Vertical','BuildingAngle','Lateral',
#                             'DrillActive','DrillPause','TripIn','TripOut','Other'),
#              fill=c('grey','magenta','blue','green','black','cyan','yellow2','red'),cex=1)
#       # Temporary plot of drill inclination to verify approach FHS 3/14/2016
#       plot(c(dt$time[1],dt$time[nrow(dt)]),c(0,100),
#            pch='.',main=sprintf('%s Hole Inclination Drill Rig %s',prm.sc$title,dt$Rig[1]),
#            ylab='Hole Inclination', ylim=c(0,100),col='black',xlab='time')
#       points(dt$time[dt$inclinationState=="Vertical"],dt$EDR_Inclination[dt$inclinationState=="Vertical"],pch='.',col='grey')
#       points(dt$time[dt$inclinationState=="BuildingAngle"],dt$EDR_Inclination[dt$inclinationState=="BuildingAngle"],pch='.',col='magenta')
#       points(dt$time[dt$inclinationState=="Lateral"],dt$EDR_Inclination[dt$inclinationState=="Lateral"],pch='.',col='blue')

##########################################################################
# Fourth, create the strip charts
if (prm.sc$from < 1 | prm.sc$to > nrow(dt)) prm.sc$from <- 1
if (prm.sc$to < prm.sc$from | prm.sc$to > nrow(dt)) prm.sc$to <- nrow(dt)

# Fix order of display for rig_state1
dt[[prm.sc$rig_state]] <- as.character(dt[[prm.sc$rig_state]])
dt[[prm.sc$rig_state]] <- factor(dt[[prm.sc$rig_state]],levels=prm.sc$dv$rig_state)

# Consolidate factor level as listed in prms
prm.sc$cs$old <- as.character(prm.sc$cs$old)
prm.sc$cs$new <- as.character(prm.sc$cs$new)
for (c in unique(prm.sc$cs$ddname)) {
  if (sum(names(dt) %in% c)) {
    dt[[c]] <- as.character(dt[[c]])
    for (i in 1:nrow(prm.sc$cs)) {
      if (prm.sc$cs$ddname[i]==c) {
        if (sum(dt[[c]]==prm.sc$cs$old[i],na.rm=T)>0) {
          cat(c,'" changed ',sum(dt[[c]]==prm.sc$cs$old[i],na.rm=T),' instances of ',
              prm.sc$cs$old[i],' to ',prm.sc$cs$new[i],'\n')
          dt[[c]][dt[[c]]==prm.sc$cs$old[i]] <- prm.sc$cs$new[i]
        }
      }
    }
    dt[[c]] <- as.factor(dt[[c]])
    cat('\nInstance count for ',c,'\n')
    print(table(dt[[c]]))
  }
}

for (i in seq(from=prm.sc$from, to=prm.sc$to, by=prm.sc$interval)) {
  stripchart(dt,from=i,to=(i+prm.sc$interval-1),prm=prm.sc)
}

#########################################################################
# Last, list the strip chart parameters
cat('\nVerbatim listing of strip chart parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',96),collapse=''),'\n')
for (i in 1:length(prm.sc$text)) { cat(prm.sc$text[i],'\n') }
cat(paste(rep('-',96),collapse=''),'\n')

stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')

if(!(prm.sc$plotFilename=='')) dev.off()
