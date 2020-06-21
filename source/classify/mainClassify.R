#######################################################################################
# Rig Classify V3.0 - main routine for rig state classification
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 28, 2019
#######################################################################################

if (sink.number()>0) sink(file=NULL)
start.time <- Sys.time()

# Source code directory absolute address
sourceDir <- "E:/Analytics/Rcode/190401_master/"

source(paste0(sourceDir,"source/util/prm.decode.R"))
source(paste0(sourceDir,"source/util/slope.R"))
source(paste0(sourceDir,"source/util/runmeanA.R"))
source(paste0(sourceDir,"source/util/ftd.R"))
source(paste0(sourceDir,"source/classify/get.prm.t.R"))
source(paste0(sourceDir,"source/classify/dbuild.R"))
source(paste0(sourceDir,"source/classify/get.prm.rc.R"))
source(paste0(sourceDir,"source/classify/dreport.R"))
source(paste0(sourceDir,"source/classify/postprocess.R"))
source(paste0(sourceDir,"source/classify/postprocess2.R"))
source(paste0(sourceDir,"source/classify/performancereports.R"))
source(paste0(sourceDir,"source/classify/activity.R"))

# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  prmFilename <- arguments[1]
} else {
  prmFilename <- paste0(sourceDir,'parms/dev/classify/classify.prm')
  cat("\nNo command line argument ... using default parameters filename ",prmFilename,"\n\n")
}

# Get the classifying parameters
cat('\nLoading classifying parameters from file ',prmFilename,'\n')
prm.rc <- get.prm.rc(prmFilename)

cat('\nLoading previous training parameters from file ',prm.rc$trainPrmFilename,'\n')
prm.t <- get.prm.t(prm.rc$trainPrmFilename)

if (nchar(prm.rc$outputFilename)>0) {
  cat('\nClassifying Rig States and sending output to:\n',prm.rc$outputFilename,'\n')
  sink(file=prm.rc$outputFilename)
  options(width=132)
}

cat(prm.rc$version,'\nRun Started at : ',as.character(start.time),'\n')

cat('\nVerbatim listing of classification parameters file from:\n',prmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.rc$text)) { cat(prm.rc$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

cat('\nVerbatim listing of training parameters file from:\n',prm.rc$trainPrmFilename,'\n\n')
cat(paste(rep('-',80),collapse=''),'\n')
for (i in 1:length(prm.t$text)) { cat(prm.t$text[i],'\n') }
cat(paste(rep('-',80),collapse=''),'\n')

cat('\nProcessing for rig state classification begins here...\n')

# Load the classification data file
cat('\nReading rig state classification data file: \n',prm.rc$rcInFilename,'\n')
de <- read.csv(prm.rc$rcInFilename,nrows=-1)
cat('Read ',nrow(de),' rows and ',ncol(de),' columns.\n')

# Checks that required EDR columns are present for post processing and performance reporting computations
errorFlag <- FALSE
if (prm.rc$ppflag) {
  if (!('EDR_BitDepth' %in% colnames(de))) {
    cat('\nFatal Error... missing input column EDR_BitDepth that is required for post processing computations')
    errorFlag <- TRUE
  }
}
if (nchar(prm.rc$connReportFilename)>0 | nchar(prm.rc$tripReportFilename)>0 | 
    nchar(prm.rc$connDetailFilename)>0 | nchar(prm.rc$tripConnDetailFilename)>0) {
  if (!('EDR_BitDepth' %in% colnames(de))) {
    cat('\nFatal Error... missing input column EDR_BitDepth that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_HoleDepth' %in% colnames(de))) {
    cat('\nFatal Error... missing input column EDR_HoleDepth that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_BlockHeight' %in% colnames(de))) {
    cat('\nFatal Error... missing input column EDR_BlockHeight that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_ROP' %in% colnames(de))) {
    cat('\nFatal Error... missing input column EDR_ROP that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (nchar(prm.t$bitDistFromHoleBottomName)<=0) {
    cat('\nFatal Error... bit distance from hole bottom predictor not used and is needed for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_HoleDepth_runmean' %in% prm.t$tpnames)) {
    cat('\nFatal Error... missing calculated EDR_HoleDepth_runmean predictor that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_BitDepth_runmean'  %in% prm.t$tpnames)) {
    cat('\nFatal Error... missing calculated EDR_BitDepth_runmean predictor that is required for performance reporting computations')
    errorFlag <- TRUE
  }
  if (!('EDR_HoleDepth_delta'  %in% prm.t$tpnames)) {
    cat('\nFatal Error... missing calculated EDR_HoleDepth_delta predictor that is required for performance reporting computations')
    errorFlag <- TRUE
  }
}
if (errorFlag) stop('Missing input columns needed for post processing and reporting computations')


if (sum(colnames(de) %in% prm.t$target) !=0 ) {
  cat('\nWARNING!!! classification target column "', prm.t$target, 
      '" already present in data to be classified.\nIt will be deleted before processing.\n\n')
  de <- de[,!(colnames(de) %in% prm.t$target)]
}
if (sum(colnames(de) %in% 'rfvote') !=0 ) {
  cat('\nWARNING!!! classification random forest vote column "rfvote"', 
      ' already present in data to be classified.\nIt will be deleted before processing.\n\n')
  de <- de[,!(colnames(de) %in% 'rfvote')]
}

# If missing EDR_TorqueValue column option selected and column is missing then
# creates column with numeric value used in model, FHS 3/8/2019
if (prm.t$missingTorque) {
  if (is.null(de$EDR_RotaryTorque)) {
    de$EDR_RotaryTorque <- prm.t$missingTorqueValue
    cat('\n\nEDR_RotaryTorque column missing and option selected to create column with value=',
        prm.t$missingTorqueValue,'\n\n')
  } else {
    if (sum(is.na(de$EDR_RotaryTorque))>0) {
      cat('\n\nEDR_RotaryTorque column has ',sum(is.na(de$EDR_RotaryTorque)),' missing values, option selected to replace them with value=',
          prm.t$missingTorqueValue,'\n\n')
      de$EDR_RotaryTorque[is.na(de$EDR_RotaryTorque)] <- prm.t$missingTorqueValue
    }
  }
} 

cat('\nLoading the Random Forest Classification Model (binary file) : \n',prm.t$rfFilename)
load(file=prm.t$rfFilename)
cat('\nThe model had ',length(rf$y),' training observations.')
cat('\nParameters indicate that model has ',prm.t$ntree*prm.t$rfchunks,' trees build in ',
    prm.t$rfchunks,' chunks of ',prm.t$ntree,' trees.\n')
cat('\nRandom Forest Decision Tree Classification Proportional Votes
      rows=predicted classification categories
    columns=random forest decision tree voting categories\n')
df <- aggregate(.~y,data=cbind(y=rf$predicted,as.data.frame(rf$votes)),
                FUN=function(x){mean(x)})
rownames(df) <- df$y
df <- as.matrix(df[,2:ncol(df)])
df <- cbind(df,vote.confidence=rowSums(df * diag(ncol(df))))
df <- cbind(df,count=as.vector(table(rf$predicted)))
df[,1:(ncol(df)-1)] <- round(df[,1:(ncol(df)-1)],digits=3)
print(df)
cat('Overall Random Forest Classification Training Vote Confidence=',
    round(sum(df[,(ncol(df)-1)]*df[,ncol(df)])/sum(df[,ncol(df)]),digits=3),'\n')

if (sum(prm.t$cp$eigencount)>0) {
  cat('\nLoading the Eigen Vector Model (binary file) : \n',prm.t$evFilename)
  load(file=prm.t$evFilename)
  cat('\nThere are eigenvectors for the following ',length(names(eigenRes)),' predictors:\n')
  print(names(eigenRes))
}

# Check for consistency between random forest and eigen model predictors and parameters file selection
errflag <- FALSE
if (length(rownames(rf$importance)) > sum(rownames(rf$importance) %in% prm.t$tpnames)) {
  cat('\nFATAL ERROR... the following random forest model predictors are missing in the parameters file selection:\n')
  print(rownames(rf$importance)[!rownames(rf$importance) %in% prm.t$tpnames])
  errflag <- TRUE
}
if (length(prm.t$tpnames) > sum(prm.t$tpnames %in% rownames(rf$importance))) {
  cat('\nFATAL ERROR... the following selected parameters file predictors are missing in the random forest model:\n')
  print(prm.t$tpnames[!prm.t$tpnames %in% rownames(rf$importance)])
  errflag <- TRUE
}
if (sum(prm.t$cp$eigencount)>0) {
  if (length(names(eigenRes)) > sum(names(eigenRes) %in% prm.t$cp$name[prm.t$cp$eigencount>0])) {
    cat('\nFATAL ERROR... the following eigen model predictors are missing in the parameters file eigen selection:\n')
    print(names(eigenRes)[!names(eigenRes) %in% prm.t$cp$name[prm.t$cp$eigencount>0]])
    errflag <- TRUE
  }
  if (length(prm.t$cp$name[prm.t$cp$eigencount>0]) > sum(prm.t$cp$name[prm.t$cp$eigencount>0] %in% names(eigenRes))) {
    cat('\nFATAL ERROR... the following selected parameters file eigen predictors are missing in the eigen model:\n')
    print(prm.t$cp$name[prm.t$cp$eigencount>0][!prm.t$cp$name[prm.t$cp$eigencount>0] %in% names(eigenRes)])
    errflag <- TRUE
  }
}

if (errflag) stop('Inconsistency between Random Forest and/or Eigen model predictors and parameters file selection')




cat('\nBuilding predictor feature variables.\n')

dtb <- tryCatch({
  dbuild(de,prm.t=prm.t,eigenRes)
},
error=function(e) {
  cat('\n\nERRORS in dbuild, UNABLE TO PERFORM RIG STATE CLASSIFICATION:\n',e$message,'\n\n')
  dtb <- NULL
})

# Only continue processing if dbuild was successful
if (!is.null(dtb)) {
  # Calculate and list summary statistics by predictor, FHS March 8, 2019
  # Initialize cumulative statistics that are kept for each predictor
  predictorStats <- data.frame(n=rep(0,length(prm.t$cp$name)),          # number of non-NA records retreived
                               mean=rep(0,length(prm.t$cp$name)),       # mean of values
                               std=rep(0,length(prm.t$cp$name)),        # standard deviation of values
                               min=rep(1e9,length(prm.t$cp$name)),      # minimum of values
                               max=rep(-1e9,length(prm.t$cp$name)),     # maximum of values
                               NAcount=rep(0,length(prm.t$cp$name)),    # number of missing records
                               Start_UOM=as.character(rep('UNK',length(prm.t$cp$name))),  # Original UOM
                               Final_UOM=as.character(rep('-',length(prm.t$cp$name))),    # FInal Units Of Measure
                               formula=as.character(rep('-',length(prm.t$cp$name))))      # formulat for UOM conversion
  rownames(predictorStats) <- prm.t$cp$name
  
  for (i in 1:length(prm.t$cp$name)) {
    c <- prm.t$cp$name[i]
    select.current <- !is.na(dtb[,c])
    predictorStats$n[i] <- sum(select.current)
    predictorStats$mean[i] <- mean(dtb[select.current,c])
    predictorStats$std[i] <- sd(dtb[select.current,c])
    predictorStats$min[i] <- min(dtb[select.current,c])
    predictorStats$max[i] <- max(dtb[select.current,c])
    predictorStats$NAcount[i] <- sum(is.na(dtb[,c]))
    # Check current predictor is in UOM list
    if (c %in% prm.t$standardUOM$dColName) {
      # Get the corresponding UOM column for the current predictor column c
      uomColName <- prm.t$standardUOM$uomColName[prm.t$standardUOM$dColName %in% c][1]
      if (uomColName %in% colnames(de)) { # make sure UOM column exists in incoming data
        predictorStats$Start_UOM <- as.character(predictorStats$Start_UOM) # set as character to avoid factor problems
        # Make sure that any missing UOM values don't cause problems
        select.current.UOM <- !is.na(de[,uomColName])
        predictorStats$Start_UOM[i] <- as.character(unique(de[select.current.UOM,uomColName]))[1] # pre-conversion UOM
        if ( unique(de[select.current.UOM,uomColName])[1] != unique(dtb[select.current.UOM,uomColName])[1]) {
          predictorStats$Final_UOM <- as.character(predictorStats$Final_UOM)
          predictorStats$Final_UOM[i] <- as.character(unique(dtb[select.current.UOM,uomColName]))[1] # post-conversion UOM
          # finds UOM conversion row with formula
          UOM.conversion.row <- which(prm.t$standardUOM$dColName %in% c &
                                      prm.t$standardUOM$inputUnit %in% predictorStats$Start_UOM[i] &
                                      prm.t$standardUOM$outputUnit %in% predictorStats$Final_UOM[i])
          if (length(UOM.conversion.row)>0) {
            predictorStats$formula <- as.character(predictorStats$formula)
            predictorStats$formula[i] <- prm.t$standardUOM$formula[UOM.conversion.row[1]]
          } else {
            predictorStats$formula <- as.character(predictorStats$formula)
            predictorStats$formula[i] <- "Not Found"
          }
        }
      }
    }
  }
  
  
  cat('\n\nSummary predictor statistics over',nrow(dtb),' records.')
  cat('\nn         = count of SQL query records processed')
  cat('\nmean      = mean (average) value after any UOM conversion')
  cat('\nstd       = standard deviation of values after any UOM conversion')
  cat('\nmin       = minimum value after any UOM conversion')
  cat('\nmax       = maximum value after any UOM conversion')
  cat('\nNAcount   = Count of missing values')
  cat('\nStart_UOM = Starting UOM from input file')
  cat('\nFinal_UOM = Final UOM used in rig state determination if different from starting UOM')
  cat('\nformula   = UOM conversion formula used\n')
  print(predictorStats,width=132,digits=4)

  
  if (!is.null(dtb$deltaBitDepth)) {
    if (sum(abs(dtb$deltaBitDepth)>prm.rc$deltaBitdepthThreshold,na.rm=T)>0) {
      cat('\nWARNING ... found ',sum(abs(dtb$deltaBitDepth)>prm.rc$deltaBitdepthThreshold,na.rm=T), 
          ' instances of probable bitdepth discontinuity with changes above threshold of ', 
          prm.rc$deltaBitdepthThreshold,' meters\n')
    }
  }
  cat('\nPerforming rig state classification\n')
  library(randomForest)
  dtb$ID <- 1:nrow(dtb)
  ID.test <- dtb[complete.cases(dtb[prm.t$tpnames]),'ID']
  datatest <- dtb[complete.cases(dtb[prm.t$tpnames]),prm.t$tpnames]
  rf.res <- data.frame(ID=ID.test)
  cat('\n',nrow(datatest),' complete cases being considered out of ',nrow(dtb),' total.\n')
  if (nrow(datatest)==0) {
    cat('\n\n\ ERROR ... zero complete cases for rig state classification, unable to continue..')
    cat('\nDiagnostic listing of non-missing values and percentage by predictor variable follows\n')
    diag <- data.frame(Predictor=NULL, Non_Missing_Count=NULL, Percent=NULL)
    for (i in 1:length(prm.t$tpnames)) {
      diag <- rbind(diag,data.frame(Predictor=prm.t$tpnames[i],
                                    Non_Missing_Count=sum(!is.na(dtb[[prm.t$tpnames[i]]])),
                                    Percent=round(100*sum(!is.na(dtb[[prm.t$tpnames[i]]]))/nrow(dtb),digits=1)))
    }
    print(diag)
    dtb <- NULL
  }
}  
  
if (!is.null(dtb)) { 
  # check to make sure that all predictors needed by model rf are present in datatest
  if (sum(!rownames(rf$importance) %in% colnames(datatest))>0) {
    cat('\n\nFATAL ERROR, DATASET MISSING PREDICTORS REQUIRED BY MODEL ....')
    missingPredictors <- rownames(rf$importance)[!rownames(rf$importance) %in% colnames(datatest)]
    cat('\nREQUIRED MISSING PREDICTORS=(',missingPredictors,')\n\n')
    stop('DATA MISSING REQUIRED MODEL PREDICTORS...')
  }
  
  rf.res[[prm.t$target]] <- predict(rf,datatest)

  # Look at random forest votes
  rf.votes <- as.data.frame(predict(rf,datatest,type="vote"))
  rf.res$rfvote <- 0
  for (c in colnames(rf.votes)) {
    rf.res$rfvote[rf.res[[prm.t$target]]==c] <- rf.votes[[c]][rf.res[[prm.t$target]]==c]
  }

  rf.vote.summary <- data.frame(as.data.frame(table(rf.res[[prm.t$target]]))); names(rf.vote.summary) <- c('names','count')
  vote_min <- aggregate(rfvote~get(prm.t$target),data=rf.res,FUN=function(x){min(x)}); names(vote_min) <- c('names','vote_min')
  vote_mean <- aggregate(rfvote~get(prm.t$target),data=rf.res,FUN=function(x){mean(x)}); names(vote_mean) <- c('names','vote_mean')
  vote_max <- aggregate(rfvote~get(prm.t$target),data=rf.res,FUN=function(x){max(x)}); names(vote_max) <- c('names','vote_max')
  rf.vote.summary <- merge(rf.vote.summary, vote_min, by='names',all.x=TRUE)
  rf.vote.summary <- merge(rf.vote.summary, vote_mean, by='names',all.x=TRUE)
  rf.vote.summary <- merge(rf.vote.summary, vote_max, by='names',all.x=TRUE)
  rownames(rf.vote.summary) <- rf.vote.summary$names
  rf.vote.summary <- rf.vote.summary[,2:ncol(rf.vote.summary)]

  cat('\nRandom Forest Rig State Classification Results with Votes Confidences\n')
  print(rf.vote.summary)
  
  cat('\nRandom Forest Decision Tree Classification Proportional Votes
        rows=predicted classification categories
        columns=random forest decision tree voting categories\n\n')
  df <- data.frame(names=as.data.frame(table(rf.res[[prm.t$target]]))[,1])
  df <- merge(df,aggregate(.~names,data=cbind(names=rf.res[[prm.t$target]],as.data.frame(rf.votes)),
                  FUN=function(x){mean(x)}),by='names',all.x=TRUE)
  rownames(df) <- df$names
  df <- as.matrix(df[,2:ncol(df)])
  df <- cbind(df,vote.confidence=rowSums(df * diag(ncol(df))))
  df <- cbind(df,count=as.vector(table(rf.res[[prm.t$target]])))
  df[,1:(ncol(df)-1)] <- round(df[,1:(ncol(df)-1)],digits=3)
  print(df)

  cat('\nOverallRFVote=',
      round(sum(df[,(ncol(df)-1)]*df[,ncol(df)],na.rm=T)/sum(df[,ncol(df)]),digits=3),
      ' Rig=',if(is.null(dtb$Rig)) '"NA"' else paste0('"',as.character(dtb$Rig[1]),'"'),
      ' WellNum=',if(is.null(dtb$WellNum)) '"NA"' else paste0('"',as.character(dtb$WellNum[1]),'"'),
      ' Date=',as.character(strptime(dtb$time[1],"%Y-%m-%d",tz="")),
      ' Obs=',nrow(rf.res),
      ' Obs_BelowRFVoteThreshold=',sum(rf.res$rfvote<prm.rc$rfvotehreshold),
      ' MaxDepth=',max(dtb$EDR_HoleDepth,na.rm=T),
      '\n')

  dt <- merge(dtb,rf.res,by="ID",all.x=TRUE)
  
  # Assigns values to rig state and rfvote NA values
  dt$rfvote[is.na(dt$rfvote)] <- 0 # Sets any rfvote NAs to zero
  dt[[prm.t$target]] <- as.character(dt[[prm.t$target]])
  dt[[prm.t$target]][is.na(dt[[prm.t$target]])] <- 'Data_Incomplete'
  dt[[prm.t$target]] <- as.factor(dt[[prm.t$target]])

  # rfvotes to be used in post processing re-assignments or saved to diagnostics file
  if (prm.rc$ppflag==TRUE | prm.rc$rfvAllSave==TRUE) {
    # If there were incomplete observations for rf, must fill in blanks with rf.votes
    if (nrow(rf.votes)<nrow(dt)) {
      rf.votes <- cbind(rf.res$ID,rf.votes)
      colnames(rf.votes) <- c('ID',colnames(rf.votes)[2:ncol(rf.votes)])
      rf.votes <- merge(data.frame(ID=dtb$ID),rf.votes,by="ID",all.x=TRUE)
      rf.votes <- rf.votes[order(rf.votes$ID),] # sort by ascending ID number
      rf.votes[is.na(rf.votes)] <- 0 # missing rfvote values are set to zero
    }
  }
  
  if (prm.rc$rfvAllSave==TRUE) {
    cat('\nIncluding individual rig state rfvotes in diagnostics file.\n')
    for (c in colnames(rf.votes)) {
      dt[[paste0('rfv_',c)]] <- rf.votes[[c]]
      cat(paste0('rfv_',c),' ')
    }
    cat('\n')
  }
  
  # Post processing option rig state consolidation and name changes
  if (prm.rc$ppflag==TRUE) {
    cat('\nPerforming post processing consolidation of classifications\n')
    rep <- dreport(dt,prm.t,prm.t$target)
    dt <- postprocess(dt,rep,prm.rc,prm.t,rf.votes)
    dt <- postprocess2(dt,prm.t,prm.rc)
  }
  
  cat('\nThe rig state classification quantities are as follows:')
  if (sum(is.na(dt[[prm.t$target]]))>0) cat('\nThere are ',sum(is.na(dt[[prm.t$target]])),' observations without rig state classifications (NA values).')
  print(table(dt[[prm.t$target]]))
  cat('\nOriginal data nrow(de)=',nrow(de),' rig classification data nrow(dt)=',nrow(dt),'\n')
  
  # Creating activity column for performance reporting
  if (nchar(prm.rc$connReportFilename)>0 | nchar(prm.rc$tripReportFilename)>0 | 
      nchar(prm.rc$connDetailFilename)>0 | nchar(prm.rc$tripConnDetailFilename)>0) {
    dt <- activity(dt,prm.t,prm.rc)
  }
  
  cat('\nMerging rig state classifications with original data.\n')
  de$time <- as.character(de$time)
  dt$time <- as.character(dt$time)
  if (nrow(de) != length(unique(de$time))) {
    cat('WARNING!!! there are ',nrow(de)-length(unique(de$time)),
        ' non-unique time values in original data.\n')
  }
  if (nrow(dt) != length(unique(dt$time))) {
    cat('WARNING!!! there are ',nrow(dt)-length(unique(dt$time)),
        ' non-unique time values in rig state classification data.\n')
  }
  # rig state and performance reports are now generated from the original data de
  # with original units of measure - FHS 4/20/16
  # dreport and performancereports require the calculated fields, timespan & activity
  # which are merged back into de from dt
  
  if (nchar(prm.rc$connReportFilename)>0 | nchar(prm.rc$tripReportFilename)>0 | 
      nchar(prm.rc$connDetailFilename)>0 | nchar(prm.rc$tripConnDetailFilename)>0) {
    reportCalcFields <- c('timespan','activity')
  } else {
    reportCalcFields <- c('timespan')
  }
  
  if (prm.rc$ppflag==TRUE & !is.null(dt[[prm.rc$target_pp]])) {
    de <- merge(de,dt[,c('time',prm.t$target,'rfvote',prm.rc$target_pp,reportCalcFields)],by='time') 
  } else {
    de <- merge(de,dt[,c('time',prm.t$target,'rfvote',reportCalcFields)],by='time') 
  }
  if (prm.t$timeColName != 'time') de <- de[,!(colnames(de) %in% 'time')] # drop the extra 'time' column

  # Write merged rig classification data with original data if requested (non-blank filename)
  if (nchar(prm.rc$rcOutFilename) >0 ) {
    cat('\nWriting merged Rig State data with ',nrow(de), 'rows and ',ncol(de), 
        ' columns (csv file) : \n',prm.rc$rcOutFilename,'\n')
    write.csv(de[,!(colnames(de) %in% reportCalcFields)],
              file=prm.rc$rcOutFilename,row.names=FALSE)
  }

  # Write diagnostic (all) classification Rig State data if requested (non blank filename)
  if (nchar(prm.rc$rcDiagFilename) > 0) {
    cat('\nWriting diagnostic classification Rig State data with ',nrow(dt), 'rows and ',ncol(dt), 
        ' columns (csv file) : \n',prm.rc$rcDiagFilename,'\n')
    write.csv(dt,file=prm.rc$rcDiagFilename,row.names=FALSE)
  }

  # Generate Rig State Report if requested in parameters (non-blank filename)
  if (nchar(prm.rc$rcReportFilename) >0 ) {
    cat('\nGenerating Rig State Classification Report\n')
    if (!is.null(de[[prm.rc$target_pp]])) rep <- dreport(de,prm.t,prm.rc$target_pp) else 
      rep <- dreport(de,prm.t,prm.t$target)

    rep <- rep[,1:8] # only save the requested columns

    cat('\nNumbers of rig state events\n')
    print(table(rep$rig_state))

    cat('\nWriting Rig State Classification Report (csv file) : \n',prm.rc$rcReportFilename,'\n')
    rep$startdatetime <- as.character(rep$startdatetime)
    rep$enddatetime <- as.character(rep$enddatetime)
    write.csv(rep,file=prm.rc$rcReportFilename,row.names=FALSE)
  }

  # Generate and save performance reports if requested in parameters (non-blank filenames)
  if (nchar(prm.rc$connReportFilename)>0 | nchar(prm.rc$tripReportFilename)>0 | 
      nchar(prm.rc$connDetailFilename)>0 | nchar(prm.rc$tripConnDetailFilename)>0) {
    # fix for NA EDR_ROP in original data (de) that was set to zero in dbuild with condition test in (dt)
    # need to transfer this to de so that performancereports doesn't drop these records. FHS Sep 12, 2017
    if (length(which(is.na(de$EDR_ROP) & dt$EDR_ROP==0))>0) de$EDR_ROP[which(is.na(de$EDR_ROP) & dt$EDR_ROP==0)] <- 0
    
    cat('\nGenerating rig performance reports\n')
    perfrep <- performancereports(de,prm.t,prm.rc)
  
    if (nchar(prm.rc$connReportFilename)>0) {  
      cat('\nWriting Connecting Time Performance Report (csv file) : \n',prm.rc$connReportFilename,'\n')
      write.csv(perfrep$repconnect,file=prm.rc$connReportFilename,row.names=FALSE)
      print(perfrep$repconnect[,!(colnames(perfrep$repconnect) %in% 'Remark')])
    }
    
    if (nchar(prm.rc$connDetailFilename)>0) {  
      cat('\nWriting Connecting Time Detail Performance Report (csv file) : \n',prm.rc$connDetailFilename,'\n')
      write.csv(perfrep$repconnectdetail,file=prm.rc$connDetailFilename,row.names=FALSE)
      print(perfrep$repconnectdetail)
    }
  
    if (nchar(prm.rc$tripReportFilename)>0) {  
      cat('\nWriting Tripping Time Performance Report (csv file) : \n',prm.rc$tripReportFilename,'\n')
      write.csv(perfrep$reptspeed,file=prm.rc$tripReportFilename,row.names=FALSE)
      print(perfrep$reptspeed[,!(colnames(perfrep$reptspeed) %in% 'Remark')])
    }
    
    if (nchar(prm.rc$tripConnDetailFilename)>0) {  
      cat('\nWriting Trippin Connect Time Detail Performance Report (csv file) : \n',prm.rc$tripConnDetailFilename,'\n')
      write.csv(perfrep$reptripconndetail,file=prm.rc$tripConnDetailFilename,row.names=FALSE)
      print(perfrep$reptripconndetail[,!(colnames(perfrep$reptripconndetail) %in% 'Remark')])
    }
  }
  
  if(!is.null(dt$activity)) {
    cat('\n\nTable of observation count for ',prm.t$target,'(rows) versus activity (cols).')
    print(table(dt[[prm.t$target]],dt$activity))
  }
}

stop.time <- Sys.time()
cat('\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(stop.time,start.time,units='secs')),digits=1),' seconds.\n')

if (sink.number()>0) sink(file=NULL)
cat('\nDone.\n')
