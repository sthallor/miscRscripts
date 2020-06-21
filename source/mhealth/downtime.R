#######################################################################################
# downtime.R - looking at EDR rig downtime data
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 6, 2016
#######################################################################################

dirName <- '/Users/Fred.Seymour/EDR_Data/160506_Downtime/'
inputFilename <- '160506_Downtime.csv'
outputFilename <- '160506_log.txt'
tableFilename <- 'downtimes.csv'

inputFilename <- paste0(dirName,inputFilename)
outputFilename <- paste0(dirName,outputFilename)
tableFilename <- paste0(dirName,tableFilename)

cat('EDR Downtime analysis sending output to:\n',outputFilename,'\n')
sink(file=outputFilename)
options(width=120)

# Historian installation history
historian <- data.frame(rig=NULL,historianStart=NULL)
historian <- rbind(historian,data.frame(rig=148,historianStart="2015-07-01"))
historian <- rbind(historian,data.frame(rig=151,historianStart="2015-11-24"))
historian <- rbind(historian,data.frame(rig=769,historianStart="2015-11-25"))
historian <- rbind(historian,data.frame(rig=155,historianStart="2015-11-25"))
historian <- rbind(historian,data.frame(rig=774,historianStart="2015-12-10"))
historian <- rbind(historian,data.frame(rig=773,historianStart="2015-12-11"))
historian <- rbind(historian,data.frame(rig=156,historianStart="2016-01-13"))
historian <- rbind(historian,data.frame(rig=161,historianStart="2016-01-15"))
historian <- rbind(historian,data.frame(rig=162,historianStart="2016-02-13"))
historian <- rbind(historian,data.frame(rig=119,historianStart="2016-03-18"))
historian <- rbind(historian,data.frame(rig=135,historianStart="2016-03-18"))
historian <- rbind(historian,data.frame(rig=121,historianStart="2016-03-19"))
historian <- rbind(historian,data.frame(rig=152,historianStart="2016-03-20"))
historian <- rbind(historian,data.frame(rig=775,historianStart="2016-03-29"))
historian <- rbind(historian,data.frame(rig=157,historianStart="2016-04-16"))


inputFilename <- '/Users/Fred.Seymour/EDR_Data/160506_Downtime/160506_Downtime.csv'
cat('\nReading selected downtime rig observation from file: \n',inputFilename)
dt <- read.csv(inputFilename,nrow=-1)
cat('\nRead ',nrow(dt),' rows and ',ncol(dt),' columns.')

cat('\nDrillingDesc "Downtime" count=',length(grep('Downtime',as.character(dt$DrillingDesc))))
cat('\nFiltering to just include observations with "Downtime" in DrillingDesc')
dt <- dt[grep('Downtime',as.character(dt$DrillingDesc)),]
cat('\nNow have ',nrow(dt),' rows and ',ncol(dt),' columns.')
dt$DrillingDesc <- as.factor(as.character(dt$DrillingDesc))
cat('\nThis represents approximately ',round(nrow(dt)/360),' Hours of downtime')

cat('\n\nlength(unique(dt$DrillingDesc))=',length(unique(dt$DrillingDesc)))

cat('\n\nTable of observation counts for Downtime DrillingDesc\n\n')
tableDrillingDesc <- as.data.frame(table(dt$DrillingDesc))
colnames(tableDrillingDesc) <- c('DrillingDesc','Count')
tableDrillingDesc$Hours <- round(tableDrillingDesc$Count/360)
tableDrillingDesc$Prop <- round(tableDrillingDesc$Count/nrow(dt),digits=3)

tableDrillingDesc$Rigs <- 0
tableDrillingDesc$Wells <- 0
tableDrillingDesc$Remarks <- 0
for (i in 1:nrow(tableDrillingDesc)) {
  tableDrillingDesc$Rigs[i] <- length(unique(dt$Rig[dt$DrillingDesc==tableDrillingDesc$DrillingDesc[i]]))
  tableDrillingDesc$Wells[i] <- length(unique(dt$WellName[dt$DrillingDesc==tableDrillingDesc$DrillingDesc[i]]))
  tableDrillingDesc$Remarks[i] <- length(unique(dt$Remark[dt$DrillingDesc==tableDrillingDesc$DrillingDesc[i]]))
}

tableDrillingDesc <- tableDrillingDesc[order(tableDrillingDesc$Count,decreasing=TRUE),]

print(tableDrillingDesc)

cat('\nTable of observation counts for Downtime DrillingDesc (Rows) and Rig (Cols)\n\n')
print(table(dt$DrillingDesc,dt$Rig))


# Assemble downtime 'report'
dt$time <- paste0(substr(dt$time,1,10),' ',substr(dt$time,12,19))
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
# dt$RigDescRemark <- paste(dt$Rig,dt$DrillingDesc,dt$Remark,sep=' - ')
dt$RigDesc <- paste(dt$Rig,as.character(as.numeric(dt$WellName)),dt$DrillingDesc,dt$Remark,sep='-')

rep <- data.frame(event=rle(as.vector(dt$RigDesc))$values,
                  lengths=rle(as.vector(dt$RigDesc))$lengths)
rep$from <- 0
rep$to <- 0
rep$to <- cumsum(rep$lengths)
rep$from <- rep$to - rep$lengths + 1
colnames(rep) <- c('rig','event','from','to')
rep$count <- rep$event
rep$rig <- dt$Rig[rep$from]
rep$well <- dt$WellName[rep$from]
rep$event <-dt$DrillingDesc[rep$from]
rep$remark <- dt$Remark[rep$from]
rep$startTime <- dt$time[rep$from]
rep$endTime <- dt$time[rep$to]
rep$elapsed_HR <- round(as.numeric(difftime(rep$endTime,rep$startTime,units='hours')),digits=1)
rep$count_HR <- round((rep$to - rep$from+1)/360,digits=1)
rep$from <- NULL
rep$to <- NULL

rep$historian <- rep$rig %in% historian$rig
rep$historian <- as.character(rep$historian)
rep$historian[rep$historian=='TRUE'] <- 'Yes'
rep$historian[rep$historian=='FALSE'] <- ''
rep <- merge(rep,historian,by="rig",all.x=TRUE)
rep$historian[rep$historian=='Yes' & as.numeric(difftime(rep$startTime,rep$historianStart,units='secs'))<0] <- 'Prior'
rep$historianStart <- as.character(rep$historianStart)
rep$historianStart[is.na(rep$historianStart)] <- ''


rep <- rep[order(rep$event,rep$count_HR,decreasing=TRUE),]
cat('\n\nListing of individual down times by events\n\n')
print(rep[,c('rig','event','startTime','endTime','count_HR','historian','historianStart')],row.names=FALSE)

cat('\n\nListing of sum of "Downtime - Top drive" hours for each rig that has historian data\n')
print(aggregate(.~rig,data=rep[rep$historian=='Yes' & rep$event=='Downtime - Top drive',c('rig','count_HR')],
          FUN=function(x){sum(x,na.rm=T)}))

cat('\n\nWriting ',tableFilename,' to disk')
write.csv(rep[,c('rig','well','event','remark','startTime','endTime','count_HR','historian')],file=tableFilename,row.names=FALSE)

# rep <- rep[order(rep$rig),]
# cat('\n\nListing of individual down times by rig\n\n')
# print(rep)

cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)

