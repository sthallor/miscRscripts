# Useful diagnostics for training dataset, looking for unit problems
# FHS March 31, 2019


logfile <- '/Users/Fred.Seymour/EDR_Data/190316_TrainTest/190331_16_Wells_V8.diagnostics.txt'
if (nchar(logfile)>0) {
  cat('\nSending output to:\n',logfile,'\n')
  sink(file=logfile, append=FALSE) # append=FALSE to initialize output file
  options(width=160)
}

# Load training dataset
trainfile = '/Users/Fred.Seymour/EDR_Data/190316_TrainTest/190331_16_Wells_V8.csv'
dt <- read.csv(trainfile,nrows=-1)
cat('\nLoaded ',trainfile,' with ',nrow(dt),' rows and ',ncol(dt),' columns.\n')

cat('\nColumn names\n')
print(colnames(dt))


# Look at rigs
rigs <- unique(dt$Rig)
rigStats <- data.frame(Rig=rigs,
                       From=apply(as.matrix(rigs,ncol=1),1,function(x) min(which(dt$Rig==x))),
                       To=apply(as.matrix(rigs,ncol=1),1,function(x) max(which(dt$Rig==x))),
                       Count=apply(as.matrix(rigs,ncol=1),1,function(x) length(which(dt$Rig==x))))
rigStats$Span <- rigStats$To - rigStats$From + 1
if (!is.null(dt$time)) {
  dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
  rigStats$TimeBegin=apply(as.matrix(rigs,ncol=1),1,function(x) as.character(min(dt$time[dt$Rig==x])))
  rigStats$TimeEnd=apply(as.matrix(rigs,ncol=1),1,function(x) as.character(max(dt$time[dt$Rig==x])))
} else {
  cat('\n\nNo dt$time column, unable to determine time min and max\n\n')
}
cat('\nStatistics by rig\n')
print(rigStats)

# rig_state counts by rig
rig_states <- unique(as.character(dt$rig_state))
rigStateCounts <- data.frame(Total=rep(0,length(rigs)))
rownames(rigStateCounts) <- rigs
for (rs in rig_states) {
  rigStateCounts <- cbind(rigStateCounts,apply(as.matrix(rigs,ncol=1),1,function(x) sum(dt$Rig==x & rs==as.character(dt$rig_state))))
  colnames(rigStateCounts)[ncol(rigStateCounts)] <- rs
  rigStateCounts$Total <- rigStateCounts$Total + rigStateCounts[[rs]]
}
cat('\nrig_state counts by rig.\n')
print(rigStateCounts, width=160)

cat('\n\nUOMs for entire file')
select <- 1:nrow(dt)
for (uomCol in grep('UOM',colnames(dt))) {
  cat('\nUOM col ',colnames(dt)[uomCol],' units=',unique(as.character(dt[select,uomCol])))
}

numCols <- which(apply(as.matrix(colnames(dt),ncol=1),1,function(x) is.numeric(dt[,x])))
numStats <- data.frame(Colname=colnames(dt)[numCols],
                       Min=apply(as.matrix(numCols,ncol=1),1,function(x) min(dt[select,x],na.rm=T)),
                       Max=apply(as.matrix(numCols,ncol=1),1,function(x) max(dt[select,x],na.rm=T)),
                       Mean=apply(as.matrix(numCols,ncol=1),1,function(x) mean(dt[select,x],na.rm=T)),
                       NACount=apply(as.matrix(numCols,ncol=1),1,function(x) sum(is.na(dt[select,x]))),
                       ZeroCount=apply(as.matrix(numCols,ncol=1),1,function(x) sum(dt[select,x]==0)),
                       NonZeroMean=apply(as.matrix(numCols,ncol=1),1,function(x) mean(dt[select,x][dt[select,x] != 0],na.rm=T)))
cat('\nNumeric Column statistics for entire file\n')
print(numStats)


# Look at each numeric Col for all rigs
numCols <- which(apply(as.matrix(colnames(dt),ncol=1),1,function(x) is.numeric(dt[,x])))
rigIndex <- as.matrix(cbind(rigStats$From,rigStats$To))

for (nc in numCols) {
  numStats <- data.frame(Rigname=rigStats$Rig,
                         RecordCount=rigStats$Count,
                         Min=apply(rigIndex,1,function(x) min(dt[x[1]:x[2],nc],na.rm=T)),
                         Max=apply(rigIndex,1,function(x) max(dt[x[1]:x[2],nc],na.rm=T)),
                         Mean=apply(rigIndex,1,function(x) mean(dt[x[1]:x[2],nc],na.rm=T)),
                         NACount=apply(rigIndex,1,function(x) sum(is.na(dt[x[1]:x[2],nc]))),
                         ZeroCount=apply(rigIndex,1,function(x) sum(dt[x[1]:x[2],nc][!is.na(dt[x[1]:x[2],nc])]==0)),
                         NonZeroMean=apply(rigIndex,1,function(x) mean(dt[x[1]:x[2],nc][dt[x[1]:x[2],nc] != 0],na.rm=T)))
  
  cat('\nStats by rig for numeric column ',colnames(dt)[nc],'\n')
  print(numStats)
}


# Look at each UOM Col for all rigs
for (uc in grep('UOM',colnames(dt))) {
  # cat('\nUOM col ',colnames(dt)[uomCol],' units=',unique(as.character(dt[select,uomCol])))
  uomStats <- data.frame(Rigname=rigStats$Rig,
                         RecordCount=rigStats$Count,
                         UOMCount=apply(rigIndex,1,function(x) length(unique(dt[x[1]:x[2],uc]))),
                         UOM=apply(rigIndex,1,function(x) unique(dt[x[1]:x[2],uc])[1]))
  cat('\nStats by rig for UOM column',colnames(dt)[uc],'\n')
  print(uomStats)
}

# Close output log file
if (sink.number()>0) sink(file=NULL)



