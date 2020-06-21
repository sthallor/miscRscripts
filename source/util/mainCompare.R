#######################################################################################
# mainCompare.R - compare data in two files with optional time offset 
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 18, 2019
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/get.prm.comp.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/util/compare.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

cat('\nLoading compare parameters from file ',prmFilename,'\n')
prm.comp <- get.prm.comp(prmFilename)

if (nchar(prm.comp$outputName)>0) {
  cat('\nRunning compare on two files and sending output to:\n',prm.comp$outputName,'\n')
  sink(file=prm.comp$outputName)
  options(width=132)
}

cat(prm.comp$version,'\nRun Started at : ',as.character(start.time),'\n')
cat('\nVerbatim listing of compare parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.comp$text)) { cat(prm.comp$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')


############################################################################
nrowload <- -1 #  (-1 for all).  Allows partial file loads for testing purposes

# Load the 2 data files to be compared
# Load the comparison file1 data
cat('\n\nLoading ',prm.comp$ds1name,' comparison file\n', prm.comp$file1name)
dt.1 <- read.csv(prm.comp$file1name,nrows=nrowload)
cat('\nFile loaded with ',nrow(dt.1),' rows and ',ncol(dt.1),' columns.\n')
# check that essential columns are present
errFlag <- FALSE
if ('time' %in% colnames(dt.1) == FALSE) {
  cat('\n\nERROR in compare... ',prm.comp$ds1name,' does not have a "time" column ...')
  cat('\nBe sure to run dclean before running compare ...')
  errFlag <- TRUE
}

# Load the comparison file2 data
cat('\n\nLoading ',prm.comp$ds2name,' comparison file\n', prm.comp$file2name)
dt.2 <- read.csv(prm.comp$file2name,nrows=nrowload)
cat('\nFile loaded with ',nrow(dt.2),' rows and ',ncol(dt.2),' columns.\n')
if ('time' %in% colnames(dt.2) == FALSE) {
  cat('\n\nERROR in compare... ',prm.comp$ds2name,' does not have a "time" column ...')
  cat('\nBe sure to run dclean before running compare ...')
  errFlag <- TRUE
}

# # Testing for UOM problems ... DO NOT LEAVE IN PRODUCTION!!!!!!!!!!!!!
# dt.2$EDR_ROP <- dt.2$EDR_ROP * 3600^2
# dt.2$EDR_RotaryTorque <- dt.2$EDR_RotaryTorque / 1.2e6

if (errFlag==TRUE) stop('FATAL ERROR in compare program...')

############################################################################
# Merge the two data files

# shows file1 min and max time for file1
cat('\nThe earliest ',prm.comp$ds1name,' date (before offset) is ',as.character(dt.1$time[1]))
cat('\nThe latest ',prm.comp$ds1name,' date (before offset) is ',as.character(dt.1$time[nrow(dt.1)]))

# show file1 min,median,max timespans
dt.1$timespanAfter <- 0
dt.1$timespanAfter[1:(nrow(dt.1)-1)] <- as.numeric(difftime(dt.1$time[2:nrow(dt.1)],
                                                            dt.1$time[1:(nrow(dt.1)-1)],units='secs'))
medianTimespan1 <- median(dt.1$timespanAfter[1:(nrow(dt.1)-1)])
if (prm.comp$verbose) {
  cat('\nThe minimum ',prm.comp$ds1name,' interval timespan is ',min(dt.1$timespanAfter[1:(nrow(dt.1)-1)]),' seconds.')
  cat('\nThe median  ',prm.comp$ds1name,' interval timespan is ',medianTimespan1,' seconds.')
  cat('\nThe maximum ',prm.comp$ds1name,' interval timespan is ',max(dt.1$timespanAfter[1:(nrow(dt.1)-1)]),' seconds.\n')
}
dt.1$timespanAfter <- NULL
# Append dataset name to column names (except for time) prior to merging
names(dt.1) <- paste(names(dt.1),'.',prm.comp$ds1name,sep="")
names(dt.1)[names(dt.1)==paste('time.',prm.comp$ds1name,sep="")] <- 'time'

# Show file2 min and max time and computes offset
cat('\nThe earliest ',prm.comp$ds2name,' date (before offset) is ',as.character(dt.2$time[1]))
cat('\nThe latest ',prm.comp$ds2name,' date (before offset) is ',as.character(dt.2$time[nrow(dt.2)]))
dt.2$time <- as.POSIXct(as.numeric(as.POSIXlt(dt.2$time,"%Y-%m-%d %H:%M:%S",tz=""))+prm.comp$timeOffset,
                        origin="1970-01-01 00:00:00")
dt.2$time <- as.factor(dt.2$time)
if (prm.comp$verbose) {
  cat('\nThe earliest ',prm.comp$ds2name,' date (with',prm.comp$timeOffset,' second offset) is ',as.character(dt.2$time[1]))
  cat('\nThe latest ',prm.comp$ds2name,' date (with',prm.comp$timeOffset,' second offset) is ',as.character(dt.2$time[nrow(dt.2)]))
}

# show file2 min,median,max timespans
dt.2$timespanAfter <- 0
dt.2$timespanAfter[1:(nrow(dt.2)-1)] <- as.numeric(difftime(dt.2$time[2:nrow(dt.2)],dt.2$time[1:(nrow(dt.2)-1)],units='secs'))
medianTimespan2 <- median(dt.2$timespanAfter[1:(nrow(dt.2)-1)])
if (prm.comp$verbose) {
  cat('\nThe minimum ',prm.comp$ds2name,' interval timespan is ',min(dt.2$timespanAfter[1:(nrow(dt.2)-1)]),' seconds.')
  cat('\nThe median  ',prm.comp$ds2name,' interval timespan is ',medianTimespan2,' seconds.')
  cat('\nThe maximum ',prm.comp$ds2name,' interval timespan is ',max(dt.2$timespanAfter[1:(nrow(dt.2)-1)]),' seconds.\n')
}
dt.2$timespanAfter <- NULL
# Append dataset name to column names (except for time) prior to merging
names(dt.2) <- paste(names(dt.2),'.',prm.comp$ds2name,sep="")
names(dt.2)[names(dt.2)==paste('time.',prm.comp$ds2name,sep="")] <- 'time'

#make sure that timespans are (mostly) the same between file1 and file2 
if (medianTimespan1 != medianTimespan2) {
  cat('\n\nFATAL ERROR ... median ',prm.comp$ds1name,' timespan=',medianTimespan1,
      ' not equal to median ',prm.comp$ds2name,' timespan=',medianTimespan2)
  cat('\njoin FATAL ERROR ... ',prm.comp$ds1name,' and ',prm.comp$ds2name,' timespans not compatiable ...\n')
  errFlag <- TRUE
}

dt <- merge(dt.1,dt.2,by='time',all=FALSE)
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
dt <- dt[order(dt$time),]
cat('\n\nCreated ',prm.comp$ds1name,' and ',prm.comp$ds2name,' merged dataset with ',nrow(dt),' rows and ',ncol(dt),' columns.')
cat('\nThere were ',sum(dt.2$time %in% dt.1$time),' matching records from ',prm.comp$ds1name,' and ',prm.comp$ds2name,
    ' with a ',prm.comp$timeOffset,' second offset')
cat('\nThe earliest combined date is ',as.character(dt$time[1]))
cat('\nThe latest combined date is ',as.character(dt$time[nrow(dt)]))
if (nrow(dt)<2) {
  cat('\n\nFATAL ERROR ... number of matched rows = ',nrow(dt),
      ' insufficient for comparisons.  Check offset parameter...\n')
  errFlag <- TRUE
}

if (errFlag==TRUE) stop('FATAL ERROR in compare program...')

# look at combined array restricted index min & max from parameters
inconsistenFlag <- FALSE
if (prm.comp$indexmax == -1) prm.comp$indexmax <- nrow(dt)
if (prm.comp$indexmax > nrow(dt)) prm.comp$indexmax <- nrow(dt)
if (prm.comp$indexmin < 1) prm.comp$indexmin <- 1; inconsistentFlag <- TRUE
if (prm.comp$indexmin > nrow(dt)) prm.comp$indexmin <- 1; inconsistentFlag <- TRUE
if (prm.comp$indexmin > prm.comp$indexmax) prm.comp$indexmin <- 1; inconsistentFlag <- TRUE
if (inconsistentFlag) cat('\nWARNING... inconsistency detected in restricted combined array index limits, set to (',
                          prm.comp$indexmin,',',prm.comp$indexmax,')')
if (prm.comp$indexmin != 1 | prm.comp$indexmax != nrow(dt)) {
  cat('\nRestricted combined array index limits min=',prm.comp$indexmin,' max=',prm.comp$indexmax)
}

#########################################################################################
# Compare the numeric columns

# Create data frame with one row for each selected numeric column
numres <- data.frame(NumColName=NULL,    # Numeric column name
                     NumCount.1=NULL,    # Count of numeric records from file1
                     NACount.1=NULL,     # Count of NA (missing) records from file1
                     Mean.1=NULL,        # Mean numeric record values from file1
                     STDev.1=NULL,       # Standard Deviation of numeric record values from file1
                     Median.1=NULL,      # Median of numeric record values from file1
                     Min.1=NULL,         # Minimum of numeric record values from file1
                     Max.1=NULL,         # Maximum of numeric record values from file1
                     NumCount.2=NULL,    # Count of numeric records from file2
                     NACount.2=NULL,     # Count of NA (missing) records from file2
                     Mean.2=NULL,        # Mean numeric record values from file2
                     STDev.2=NULL,       # Standard Deviation of numeric record values from file2
                     Median.2=NULL,      # Median of numeric record values from file2
                     Min.2=NULL,         # Minimum of numeric record values from file2
                     Max.2=NULL,         # Maximum of numeric record values from file2
                     MatchCount=NULL,    # Count of matching file1 and file2 numeric records
                     DeltaMean=NULL,     # Mean.1 minus Mean.2 on matching records
                     Corr=NULL)          # Correlation between matching file1 and file2 records 

cat('\n')
for (c in prm.comp$numcol$name) {
  # cat('\n\n',paste(rep('-',80),collapse=''))
  cat('\nLooking at numeric column ',c)
  c1 <- paste(c,'.',prm.comp$ds1name,sep="")
  c2 <- paste(c,'.',prm.comp$ds2name,sep="")
  
  # make sure that both columns are present in dt
  if(!(c1 %in% names(dt)) | !(c2 %in% names(dt))) {
    if (!(c1 %in% names(dt))) cat('\nColumn ',c,' missing in ',prm.comp$ds1name,' file')
    if (!(c2 %in% names(dt))) cat('\nColumn ',c,' missing in ',prm.comp$ds2name,' file')
    cat('\nskipping comparison analysis...\n\n')
  } else {
    dt.1[,c1] <- suppressWarnings(as.numeric(dt.1[,c1]))
    dt.2[,c2] <- suppressWarnings(as.numeric(dt.2[,c2]))
    dt[,c1] <- suppressWarnings(as.numeric(dt[,c1]))
    dt[,c2] <- suppressWarnings(as.numeric(dt[,c2]))
    # impose index limits from parameters
    select <- which(!is.na(dt[,c1]) & !is.na(dt[,c2]))
    select <- select[select >= prm.comp$indexmin & select <= prm.comp$indexmax]
    if (length(select)<3) {
      cat(' insufficient numeric sample match count=',length(select),' skipping...')
    } else {
      cat(' performing comparison... ')
      i1 <- prm.comp$indexmin
      i2 <- prm.comp$indexmax
      numres <- rbind(numres,data.frame(NumColName=c,   
                                        NumCount.1=sum(!is.na(dt[i1:i2,c1])),   
                                        NACount.1=sum(is.na(dt[i1:i2,c1])),     
                                        Mean.1=mean(dt[select,c1]),
                                        Stdev.1=sd(dt[select,c1]),
                                        Median.1=median(dt[select,c1]),
                                        Min.1=min(dt[select,c1]),
                                        Max.1=max(dt[select,c1]),
                                        NumCount.2=sum(!is.na(dt[i1:i2,c2])),   
                                        NACount.2=sum(is.na(dt[i1:i2,c2])),     
                                        Mean.2=mean(dt[select,c2]),
                                        Stdev.2=sd(dt[select,c2]),
                                        Median.2=median(dt[select,c2]),
                                        Min.2=min(dt[select,c2]),
                                        Max.2=max(dt[select,c2]),
                                        MatchCount=length(select),
                                        DeltaMean=mean(dt[select,c1])-mean(dt[select,c2]),
                                        Corr=suppressWarnings(cor(dt[select,c1], dt[select,c2]))))
      
      cat(' comparison complete.')
    }
  }
}
# put .ds1name and .ds2name in place of .1 .2 in numres dataframe
colnames(numres) <- gsub('.1',paste('.',prm.comp$ds1name,sep=''),names(numres))
colnames(numres) <- gsub('.2',paste('.',prm.comp$ds2name,sep=''),names(numres))
rownames(numres) <- numres$NumColName

cat('\n\nNumeric comparison results.\n')
print(numres)

# Write numeric comparison results if requested
if (nchar(prm.comp$numCompName)>0) {
  cat('\n\nWriting numeric statistics comparison file \n', prm.comp$numCompName,'\n')
  write.csv(numres,file=prm.comp$numCompName,row.names=FALSE)
}

#########################################################################################
# Compare the category columns

cat('\n')
i1 <- prm.comp$indexmin
i2 <- prm.comp$indexmax
for (c in prm.comp$catcol$name) {
  cat('\n\n',paste(rep('-',80),collapse=''))
  cat('\nLooking at category column ',c)
  c1 <- paste(c,'.',prm.comp$ds1name,sep="")
  c2 <- paste(c,'.',prm.comp$ds2name,sep="")
  
  # make sure that both columns are present in dt
  if(!(c1 %in% names(dt)) | !(c2 %in% names(dt))) {
    if (!(c1 %in% names(dt))) cat('\nColumn ',c,' missing in ',prm.comp$ds1name,' file')
    if (!(c2 %in% names(dt))) cat('\nColumn ',c,' missing in ',prm.comp$ds2name,' file')
    cat('\nskipping comparison analysis...\n\n')
  } else {
    cat('\nCategory counts for ',prm.comp$ds1name,'\n')
    print(table(dt[i1:i2,c1],useNA='always'))
    cat('\nCategory counts for ',prm.comp$ds2name,'\n')
    print(table(dt[i1:i2,c2],useNA='always'))
    cat('\nTable of matches between ', prm.comp$ds1name,' (rows) and ',prm.comp$ds2name,' (cols)\n')
    res.table <- table(dt[i1:i2,c1],dt[i1:i2,c2],useNA='ifany') 
    print(res.table)
    # res.match for category matches in res.table, initialize to zero
    res.match <- matrix(0,nrow=nrow(res.table),ncol=ncol(res.table))
    for (i in 1:nrow(res.table)) {
      # set to 1 if rowname equals column name (corresponding categories) 
      res.match[i, which(colnames(res.table) %in% rownames(res.table)[i])] <- 1
    }
    # sum the records that match between file1 and file2
    res.match.sum <- sum(res.table * res.match)
    res.all.sum <- sum(res.table)
    cat('\n',res.match.sum,' matching records out of ',res.all.sum,' total ',
        round(100*res.match.sum/res.all.sum,2),'percent.')
    # Write the file if requested
    if (nchar(prm.comp$catCompName)>0) {
      rownames(res.table) <- paste(rownames(res.table),'.',prm.comp$ds1name,sep='')
      colnames(res.table) <- paste(colnames(res.table),'.',prm.comp$ds2name,sep='')
      tempFileName <- paste(substr(prm.comp$catCompName,1,(nchar(prm.comp$catCompName)-4)),'_',c,'.csv',sep='')
      cat('\n\nWriting category comparison table file \n', tempFileName,'\n')
      write.csv(res.table,file=tempFileName)
    }
  }
}

########################################################################################
# Create scatterplots and line plots
if (!prm.comp$plotName=="") {
  cat("\nSending compareplot chart output to pdf file:\n",prm.comp$plotName,"\n")
  if (file.exists(prm.comp$plotName)) {
    # Removes file because of apparent bug in pdf reader that does not allow
    # pdf file to be overwritten after it has been openned even if it has been closed
    # FHS Nov 30, 2017
    i <- 0
    file.remove(prm.comp$plotName)
    while(file.exists(prm.comp$plotName) & i<10) {
      Sys.sleep(1)
      if (i>0) cat('\nwaiting for system to remove old pdf file i=',i)
      i <- i+1
    }
  }
  
  pdf(file=prm.comp$plotName)
}
i1 <- prm.comp$indexmin
i2 <- prm.comp$indexmax

# Scatterplots
for (c in prm.comp$numcol$name[prm.comp$numcol$scatter]) {
  c1 <- paste(c,'.',prm.comp$ds1name,sep="")
  c2 <- paste(c,'.',prm.comp$ds2name,sep="")
  if((c1 %in% names(dt)) & (c2 %in% names(dt))) {
    select <- which(!is.na(dt[,c1]) & !is.na(dt[,c2]))
    select <- select[select >= prm.comp$indexmin & select <= prm.comp$indexmax]
    if (length(select)>0 & length(which(prm.comp$numcol$name %in% c))>0) {
      i <- which(prm.comp$numcol$name %in% c)[1]
      
      pltMin <- max(prm.comp$numcol$min[i],min(dt[select,c1],dt[select,c2]))
      pltMax <- min(prm.comp$numcol$max[i],max(dt[select,c1],dt[select,c2]))
      meanX <- paste('=',as.character(signif(mean(dt[select,c1]),digits=4)),' ')
      sdX <- paste('=',as.character(signif(sd(dt[select,c1]),digits=4)))
      meanY <- paste('=',as.character(signif(mean(dt[select,c2]),digits=4)),' ')
      sdY <- paste('=',as.character(signif(sd(dt[select,c2]),digits=4)))
      subtitle <- paste('n=',as.character(length(select)),' Corr=',
                        as.character(signif(numres[which(rownames(numres) %in% c)[1],"Corr"],digits=5)))
      if (prm.comp$plotmismatch) {
        ccat <- prm.comp$catcol$name[1]
        ccat1 <- paste(ccat,'.',prm.comp$ds1name,sep="")
        ccat2 <- paste(ccat,'.',prm.comp$ds2name,sep="")
        selectmismatch <- select[which(!is.na(dt[select,c1]) & !is.na(dt[select,c2]) & 
                                         as.character(dt[select,ccat1]) != as.character(dt[select,ccat2]))]
        subtitle <- paste(subtitle,prm.comp$catcol$name[1],' mis-matches=',length(selectmismatch))
      }

      plot(dt[select,c1],dt[select,c2],xlim=c(pltMin,pltMax),ylim=c(pltMin,pltMax),
           xlab=bquote(.(paste(c1,'  ')) ~ mu ~ .(meanX) ~ sigma ~ .(sdX)),
           ylab=bquote(.(paste(c2,'  ')) ~ mu ~ .(meanY) ~ sigma ~ .(sdY)),
           main=prm.comp$title,cex=prm.comp$plotsymbolsize,col=prm.comp$ds1color,pch=prm.comp$plotpchsymbol,
           sub=subtitle)
      
      lines(c(pltMin,pltMax),c(pltMin,pltMax), col='black')
      points(mean(dt[select,c1]),mean(dt[select,c2]),col=prm.comp$ds2color,pch=19,cex=1.5)
      # Standard deviation cross
      lines(c(mean(dt[select,c1]),mean(dt[select,c1])),
            c((mean(dt[select,c2])-sd(dt[select,c2])),(mean(dt[select,c2])+sd(dt[select,c2]))),
            col=prm.comp$ds2color,lwd=2)
      lines(c((mean(dt[select,c1])-sd(dt[select,c1])),(mean(dt[select,c1])+sd(dt[select,c1]))),
            c(mean(dt[select,c2]),mean(dt[select,c2])),
            col=prm.comp$ds2color,lwd=2)
      # Standard deviation cross ends
      lines(c((mean(dt[select,c1])-sd(dt[select,c1])/10),(mean(dt[select,c1])+sd(dt[select,c1])/10)),
            c((mean(dt[select,c2])-sd(dt[select,c2])),(mean(dt[select,c2])-sd(dt[select,c2]))),
            col=prm.comp$ds2color,lwd=2)
      lines(c((mean(dt[select,c1])-sd(dt[select,c1])/10),(mean(dt[select,c1])+sd(dt[select,c1])/10)),
            c((mean(dt[select,c2])+sd(dt[select,c2])),(mean(dt[select,c2])+sd(dt[select,c2]))),
            col=prm.comp$ds2color,lwd=2)
      lines(c((mean(dt[select,c1])+sd(dt[select,c1])),(mean(dt[select,c1])+sd(dt[select,c1]))),
            c((mean(dt[select,c2])-sd(dt[select,c2])/10),(mean(dt[select,c2])+sd(dt[select,c2])/10)),
            col=prm.comp$ds2color,lwd=2)
      lines(c((mean(dt[select,c1])-sd(dt[select,c1])),(mean(dt[select,c1])-sd(dt[select,c1]))),
            c((mean(dt[select,c2])-sd(dt[select,c2])/10),(mean(dt[select,c2])+sd(dt[select,c2])/10)),
            col=prm.comp$ds2color,lwd=2)
      # Regression lines option
      if (prm.comp$plotregression) {
        regressionxy <- lm(dt[select,c2] ~ dt[select,c1])
        x1 <- pltMin - (pltMax - pltMin)
        y1 <- regressionxy$coefficients[1] + regressionxy$coefficients[2] * x1
        x2 <- pltMax + (pltMax - pltMin)
        y2 <- regressionxy$coefficients[1] + regressionxy$coefficients[2] * x2
        lines(c(x1,x2),c(y1,y2),lty=prm.comp$regressionLineType,col=prm.comp$regressionLineColor)
        
        regressionyx <- lm(dt[select,c1] ~ dt[select,c2])
        y1 <- pltMin - (pltMax - pltMin)
        x1 <- regressionyx$coefficients[1] + regressionyx$coefficients[2] * y1
        y2 <- pltMax + (pltMax - pltMin)
        x2 <- regressionyx$coefficients[1] + regressionyx$coefficients[2] * y2
        lines(c(x1,x2),c(y1,y2),lty=prm.comp$regressionLineType,col=prm.comp$regressionLineColor)
      }
      
      # mismatch symbol plot option
      if (prm.comp$plotmismatch) {
        # mismatch based on first category comparison (i.e. rig_state)
        if (length(selectmismatch)>0) {
          points(dt[selectmismatch,c1],dt[selectmismatch,c2],col=prm.comp$mismatchSymbolColor,
                 pch=prm.comp$mismatchpchsymbol,cex=prm.comp$mismatchSymbolSize)
        }
      }
      
      # setup legend parameters
      
      legendParms <- data.frame(label='Data points',
                                pch=prm.comp$plotpchsymbol,
                                color=prm.comp$ds1color)
      if (prm.comp$plotmismatch) {
        legendParms <- rbind(legendParms,data.frame(label='mis-matches',
                                                    pch=prm.comp$mismatchpchsymbol,
                                                    color=prm.comp$mismatchSymbolColor))
      }
      if (prm.comp$plotregression) {
        legendParms <- rbind(legendParms,data.frame(label='regressions',
                                                    pch=19,
                                                    color=prm.comp$regressionLineColor))
      }
      
      legendParms$label <- as.character(legendParms$label)
      legendParms$color <- as.character(legendParms$color)
      legend('topleft',c(legendParms$label,expression(paste(mu,' & ',sigma,' bars'))),
                       pch=c(legendParms$pch,19),
                       col=c(legendParms$color,prm.comp$ds2color),
                       cex=1)
    }
  }
}

# line plots
i1 <- prm.comp$indexmin
i2 <- prm.comp$indexmax
for (c in prm.comp$numcol$name[prm.comp$numcol$line]) {
  c1 <- paste(c,'.',prm.comp$ds1name,sep="")
  c2 <- paste(c,'.',prm.comp$ds2name,sep="")
  if((c1 %in% names(dt)) & (c2 %in% names(dt))) {
    select <- which(!is.na(dt[,c1]) & !is.na(dt[,c2]))
    select <- select[select >= prm.comp$indexmin & select <= prm.comp$indexmax]
    if (length(select)>0 & length(which(prm.comp$numcol$name %in% c))>0) {
      i <- which(prm.comp$numcol$name %in% c)[1]
      
      pltMin <- max(prm.comp$numcol$min[i],min(dt[select,c1],dt[select,c2]))
      pltMax <- min(prm.comp$numcol$max[i],max(dt[select,c1],dt[select,c2]))
      if (prm.comp$numcol$reverse[i]) { # reverse y-axis min & max if requested
        pltMax <- max(prm.comp$numcol$min[i],min(dt[select,c1],dt[select,c2]))
        pltMin <- min(prm.comp$numcol$max[i],max(dt[select,c1],dt[select,c2]))
      }
      subtitle <- paste('n=',as.character(length(select)),' Corr=',
                        as.character(signif(numres[which(rownames(numres) %in% c)[1],"Corr"],digits=5)))
      if (prm.comp$plotmismatch) {
        ccat <- prm.comp$catcol$name[1]
        ccat1 <- paste(ccat,'.',prm.comp$ds1name,sep="")
        ccat2 <- paste(ccat,'.',prm.comp$ds2name,sep="")
        selectmismatch <- select[which(!is.na(dt[select,c1]) & !is.na(dt[select,c2]) & 
                                         as.character(dt[select,ccat1]) != as.character(dt[select,ccat2]))]
        subtitle <- paste(subtitle,prm.comp$catcol$name[1],' mis-matches=',length(selectmismatch))
      }
      
      plot(select,dt[select,c1],ylim=c(pltMin,pltMax),
           xlab='Combined compare dataset index', ylab=c,
           main=prm.comp$title,
           col=prm.comp$ds1color,
           type='l',
           sub=subtitle)
      lines(select,dt[select,c2],col=prm.comp$ds2color)
      # mismatch symbol plot option
      if (prm.comp$plotmismatch) {
        # mismatch based on first category comparison (i.e. rig_state)
        if (length(selectmismatch)>0) {
          points(selectmismatch,dt[selectmismatch,c1],col=prm.comp$mismatchSymbolColor,
                 pch=prm.comp$mismatchpchsymbol,cex=prm.comp$mismatchSymbolSize)
          points(selectmismatch,dt[selectmismatch,c2],col=prm.comp$mismatchSymbolColor,
                 pch=prm.comp$mismatchpchsymbol,cex=prm.comp$mismatchSymbolSize)
        }
        
        legend('topleft',c(c1,c2,'mis-matches'),
               pch=c(19,19,prm.comp$mismatchpchsymbol),
               col=c(prm.comp$ds1color,prm.comp$ds2color,prm.comp$mismatchSymbolColor),
               cex=1)
        
      } else {
        legend('topleft',c(c1,c2),
               pch=c(19,19),
               col=c(prm.comp$ds1color,prm.comp$ds2color),
               cex=1)
      }
      
    }
  }
}



if(!(prm.comp$plotName=='')) dev.off()

########################################################################################
# Create combined data output file if requested
if (nchar(prm.comp$combinedName)>0) {
  # Start with the time column
  dt.save = as.data.frame(dt$time)
  colnames(dt.save) <- 'time'
  
  # Loop through the category columns
  for (c in prm.comp$catcol$name[prm.comp$catcol$save]) {
    c1 <- paste(c,'.',prm.comp$ds1name,sep="")
    c2 <- paste(c,'.',prm.comp$ds2name,sep="")
    if((c1 %in% names(dt)) & (c2 %in% names(dt))) {
      cm <- paste(c,'.match',sep='')
      dt.save <- cbind(dt.save,dt[,c1],dt[,c2],rep(0,nrow(dt))) # third column will be used as match column
      colnames(dt.save)[(ncol(dt.save)-2):ncol(dt.save)] <- c(c1,c2,cm)
      dt.save[as.character(dt.save[,c1]) == as.character(dt.save[,c2]),cm] <- 1 # all matching values set to 1
    } 
  }
  
  # Loop through the numeric columns selected for save
  for (c in prm.comp$numcol$name[prm.comp$numcol$save]) {
    c1 <- paste(c,'.',prm.comp$ds1name,sep="")
    c2 <- paste(c,'.',prm.comp$ds2name,sep="")
    if((c1 %in% names(dt)) & (c2 %in% names(dt))) {
      dt.save <- cbind(dt.save,dt[,c1],dt[,c2])
      colnames(dt.save)[(ncol(dt.save)-1):ncol(dt.save)] <- c(c1,c2)
    }
  }
  
  cat('\n\nWriting comparison .csv file with requested columns from both file1 and file2\n', prm.comp$combinedName,'\n')
  write.csv(dt.save,file=prm.comp$combinedName,row.names=FALSE)
  cat('File written with ',nrow(dt.save),' rows and ',ncol(dt.save),' columns.')
}

########################################################################################

stop.time <- Sys.time()
cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')
cat('\nDone.\n')
if (sink.number()>0) sink(file=NULL)
