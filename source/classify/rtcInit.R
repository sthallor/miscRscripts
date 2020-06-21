#######################################################################################
# rtcInit.R Real Time Rig Classify Initialization V3.0 - initialize data
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 22, 2019
#######################################################################################
rtcInit <- function(prmFilename) {
  
  # rtc, List that holds all RTclassify information
  rtc <- list()
  
  # Get the real time classifying parameters
  cat('\nLoading real time classifying parameters from file ',prmFilename,'\n')
  rtc$prm.rt <- get.prm.rt(prmFilename)
  
  cat('\nLoading previous training parameters from file ',rtc$prm.rt$trainPrmFilename,'\n')
  rtc$prm.t <- get.prm.t(rtc$prm.rt$trainPrmFilename)
  
  if (nchar(rtc$prm.rt$dcleanPrmFilename)>0) {
    cat('\nLoading data clean parameters from file ',rtc$prm.rt$dcleanPrmFilename,'\n')
    rtc$prm.dc <- get.prm.dc(rtc$prm.rt$dcleanPrmFilename)
  }
  
  if (nchar(rtc$prm.rt$outputLogFilename)>0) {
    cat('\nClassifying Rig States and sending output to:\n',rtc$prm.rt$outputLogFilename,'\n')
    sink(file=rtc$prm.rt$outputLogFilename, append=FALSE) # append=FALSE to initialize output file
    options(width=132)
  }
  
  cat(rtc$prm.rt$version,'\nRun Started at : ',as.character(start.time),'\n')
  
  cat('\nVerbatim listing of classification parameters file from:\n',prmFilename,'\n\n')
  cat(paste(rep('-',80),collapse=''),'\n')
  for (i in 1:length(rtc$prm.rt$text)) { cat(rtc$prm.rt$text[i],'\n') }
  cat(paste(rep('-',80),collapse=''),'\n')
  
  cat('\nVerbatim listing of rig state training parameters file from:\n',rtc$prm.rt$trainPrmFilename,'\n\n')
  cat(paste(rep('-',80),collapse=''),'\n')
  for (i in 1:length(rtc$prm.t$text)) { cat(rtc$prm.t$text[i],'\n') }
  cat(paste(rep('-',80),collapse=''),'\n')
  
  if (nchar(rtc$prm.rt$dcleanPrmFilename)>0) {
    cat('\nVerbatim listing of dclean parameters file from:\n',rtc$prm.rt$dcleanPrmFilename,'\n\n')
    cat(paste(rep('-',80),collapse=''),'\n')
    for (i in 1:length(rtc$prm.dc$text)) { cat(rtc$prm.dc$text[i],'\n') }
    cat(paste(rep('-',80),collapse=''),'\n')
  }
  
  cat('\nLoading the Random Forest Classification Model (binary file) : \n',rtc$prm.t$rfFilename)
  load(file=rtc$prm.t$rfFilename)
  rtc$rf <- rf
  rf <- NULL
  cat('\nThe model had ',length(rtc$rf$y),' training observations.')
  cat('\nRandom Forest Decision Tree Classification Proportional Votes
      rows=predicted classification categories
      columns=random forest decision tree voting categories\n')
  df <- aggregate(.~y,data=cbind(y=rtc$rf$predicted,as.data.frame(rtc$rf$votes)),
                  FUN=function(x){mean(x)})
  rownames(df) <- df$y
  df <- as.matrix(df[,2:ncol(df)])
  df <- cbind(df,vote.confidence=rowSums(df * diag(ncol(df))))
  df <- cbind(df,count=as.vector(table(rtc$rf$predicted)))
  df[,1:(ncol(df)-1)] <- round(df[,1:(ncol(df)-1)],digits=3)
  print(df)
  cat('Overall Random Forest Classification Training Vote Confidence=',
      round(sum(df[,(ncol(df)-1)]*df[,ncol(df)])/sum(df[,ncol(df)]),digits=3),'\n')
  
  if (sum(rtc$prm.t$cp$eigencount)>0) {
    cat('\nLoading the Eigen Vector Model (binary file) : \n',rtc$prm.t$evFilename)
    load(file=rtc$prm.t$evFilename)
    rtc$eigenRes <- eigenRes
    cat('\nThere are eigenvectors for the following ',length(names(rtc$eigenRes)),' predictors:\n')
    print(names(rtc$eigenRes))
  }
  
  # Check for consistency between random forest and eigen model predictors and parameters file selection
  errflag <- FALSE
  if (length(rownames(rtc$rf$importance)) > sum(rownames(rtc$rf$importance) %in% rtc$prm.t$tpnames)) {
    cat('\nFATAL ERROR... the following random forest model predictors are missing in the parameters file selection:\n')
    print(rownames(rtc$rf$importance)[!rownames(rtc$rf$importance) %in% rtc$prm.t$tpnames])
    errflag <- TRUE
  }
  if (length(rtc$prm.t$tpnames) > sum(rtc$prm.t$tpnames %in% rownames(rtc$rf$importance))) {
    cat('\nFATAL ERROR... the following selected parameters file predictors are missing in the random forest model:\n')
    print(rtc$prm.t$tpnames[!rtc$prm.t$tpnames %in% rownames(rtc$rf$importance)])
    errflag <- TRUE
  }
  if (sum(rtc$prm.t$cp$eigencount)>0) {
    if (length(names(rtc$eigenRes)) > sum(names(rtc$eigenRes) %in% rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0])) {
      cat('\nFATAL ERROR... the following eigen model predictors are missing in the parameters file eigen selection:\n')
      print(names(rtc$eigenRes)[!names(rtc$eigenRes) %in% rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0]])
      errflag <- TRUE
    }
    if (length(rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0]) > sum(rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0] %in% names(rtc$eigenRes))) {
      cat('\nFATAL ERROR... the following selected parameters file eigen predictors are missing in the eigen model:\n')
      print(rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0][!rtc$prm.t$cp$name[rtc$prm.t$cp$eigencount>0] %in% names(rtc$eigenRes)])
      errflag <- TRUE
    }
  }
  
  if (errflag) stop('Inconsistency between Random Forest and/or Eigen model predictors and parameters file selection')
  
  
  
  

  # new results output .csv file array, FHS Mar 12, 2019
  rtc$dt.output <- data.frame(time=NULL,
                              epoch=NULL,
                              rig_state=NULL,
                              code=NULL,
                              rfvote=NULL,
                              postProcess=NULL,
                              lagTime=NULL,
                              processTime=NULL,
                              ntry=NULL,
                              readErr=NULL,
                              nskip=NULL,
                              incomplete=NULL,
                              writeErr=NULL,
                              lagdel=NULL,
                              nrow=NULL,
                              ncol=NULL,
                              noverlap=NULL,
                              nNAraw=NULL,
                              nNAclean=NULL,
                              missMatch=NULL,
                              missInt=NULL)
  
  # rtc$errorCount <- 0 # Initialize SQL Query read error count
  rtc$dt.diag <- data.frame()
  rtc$de.previous <- NULL # diagnostic comparison with previous sqlquery
  rtc$previous.observation.time <- Sys.time() - 32000000 # 32M seconds (1yr) to trigger dependency 
  
  # Initialize sql statistics that are saved with each new record
  # rtc$sql. accumulated values through multiple calls to rtcTest, reset when record saved
  rtc$sql <- list(ntry=0,       # count of sql query trys
                  readErr=0,    # count of non-dataframe returns from sql
                  nskip=0,      # count of skipped trys with same time interval as previous record
                  incomplete=0, # incomplete principal records
                  writeErr=0)   # sql write error
  # rtc$rec.  statistics on current sql query that is recorded
  rtc$rec <- list(lagdel=0,   # Number of records deleted within lag time delay of actual time
                  nrow=0,     # Number of rows in current query
                  ncol=0,     # Number of cols in current query
                  noverlap=0, # Number of overlapping records with previous query
                  # statistics on post dclean sql query with no interpolation
                  nNAraw=0,      # NA count for all predictors before dclean interpolation/extrapolation
                  nNAclean=0,    # NA count for all predictors after clean interpolation/extrapolation
                  missMatch=0,   # non-matching numeric values with previous query for all predictors
                  # statistics on post dclean sql query with interpolation
                  missInt=0)   # Number of missed intervals since last record
                  
  # Initialize cumulative statistics that are kept for each predictor
  rtc$cumPredStats <- data.frame(n=rep(0,length(rtc$prm.t$cp$name)),          # number of non-NA records retreived
                                 s1=rep(0,length(rtc$prm.t$cp$name)),         # sum of values
                                 s2=rep(0,length(rtc$prm.t$cp$name)),         # sum of values squared
                                 mean=rep(0,length(rtc$prm.t$cp$name)),       # mean of values
                                 std=rep(0,length(rtc$prm.t$cp$name)),        # standard deviation of values
                                 min=rep(1e9,length(rtc$prm.t$cp$name)),      # minimum of values
                                 max=rep(-1e9,length(rtc$prm.t$cp$name)),     # maximum of values
                                 NAraw=rep(0,length(rtc$prm.t$cp$name)),      # number of NA records before dclean interpolations
                                 NAclean=rep(0,length(rtc$prm.t$cp$name)),    # number of clean NA records after interpolation
                                 missMatch= rep(0,length(rtc$prm.t$cp$name)), # number of missmatch between current & previous sql
                                 SQL_UOM=as.character(rep('UNK',length(rtc$prm.t$cp$name))),  # Original UOM
                                 UOM=as.character(rep('-',length(rtc$prm.t$cp$name))),    # Units Of Measure
                                 formula=as.character(rep('-',length(rtc$prm.t$cp$name))))  # formulat for UOM conversion
  rownames(rtc$cumPredStats) <- rtc$prm.t$cp$name
  
  # Sets up dclean parms with no interpolation/extrapolation
  rtc$prm.dc.zeroMaxLook <- rtc$prm.dc
  rtc$prm.dc.zeroMaxLook$valueLimits$timespan <- 0
  rtc$prm.dc.zeroMaxLook$valueLimits$maxLook <- 0
  rtc$prm.dc.zeroMaxLook$contMaxTimespan <- 0
  rtc$prm.dc.zeroMaxLook$contMaxLook <- 0
  rtc$prm.dc.zeroMaxLook$verbose <- FALSE 

  cat('\nProcessing for real time rig state classification begins here...\n')
  
  if (sink.number()>0) sink(file=NULL)
  
  return(rtc)
}