#######################################################################################
# get.prm.predictTrain.R - read and decode historian training ASCII parameters file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, April 20, 2017
#######################################################################################

# Program Parameters ASCII text template
# Program Predict Train V3.0 3/14/2017
# 
# Title  [Random Forest Training for High Value Alarm Predictions 3/14/2017]
# 
# Select training data filename by uncommenting (remove pound sign) on one line
# Training Data Filename       [/home/fhs/LargeDatasets/Ensign/170222_MachineHealth/170222_DR156_IGBT_temp_All2016_clean.csv]
# 
# Random Forest Model .bin Filename Appendix                         [_randomForestModel.bin]
# Program Output Log .txt Filename Appendix (blank to skip)          [_randomForestLog.txt]
# Sorted Predictor Importance .txt Filename Appendix (blank to skip) [_randomForestImportance.txt]
# 
# Parameters for building calculated predictors
# Date and Time Column Name                          [time]
# Prediction Target Source Column Name               [igbt_temp]
# Calculated Prediction Target Column Name           [futureIgbtMax]
# Target alarm cutoff value                          [100]
# Future prediction time span (minutes)              [20]
# Past calculated predictor time span (minutes)      [20]
# Dominant observation time interval (seconds)       [10]
# 
# Parameters for random forest training
# Number of observations used for training           [10000]
# Balance training above/below alarm cutoff (Yes/No) [Yes]
# ntree - number of Random Forest Trees              [300]
# mtry - number of predictors per decision           [3]
# do.trace - diagnostic tree interval                [25]
# Verbose log file output (Yes/No)                   [Yes]
# 
# Source Predictors from input file
# [block_height]
# [dc_bus_voltage]
# [output_voltage]
# 
# All Predictors for Training/Predicting
# [igbt_tempRunMean]
# [igbt_tempRunSD]
# [igbt_temp]

get.prm.predictTrain <- function(prmFilename) {
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  keywords <- c('Program Predict Train V3.0 3/14/2017',
                'Title',
                'Training Data Filename',
                'Random Forest Model .bin Filename Appendix (blank to skip)',
                'Program Output Log .txt Filename Appendix (blank to skip)',
                'Sorted Predictor Importance .txt Filename Appendix (blank to skip)',
                'Date and Time Column Name',
                'Prediction Target Source Column Name',
                'Calculated Prediction Target Column Name',
                'Target alarm cutoff value',
                'Future prediction time span (minutes)',
                'Past calculated predictor time span (minutes)',
                'Dominant observation time interval (seconds)',
                'Number of observations used for training',
                'Balance training above/below alarm cutoff (Yes/No)',
                'ntree - number of Random Forest Trees',
                'mtry - number of predictors per decision',
                'do.trace - diagnostic tree interval',
                'Verbose log file output (Yes/No)',
                'Source Predictors from input file',
                'All Predictors for Training/Predicting')
  
  # Initialize parameters list
  prm.pt <- list()
  # copies text parameters file verbatim
  prm.pt$text <- text$text
  
  # Program Historian Predict Train V3.0 3/14/2017
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.pt$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$title <- text$v1[r]
  
  # Training Data Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$trainFilename <- text$v1[r]
  if(!(substr(prm.pt$trainFilename,(nchar(prm.pt$trainFilename)-3),nchar(prm.pt$trainFilename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... training file must be of type ".csv"')
  }
  
  # Random Forest Model .bin Filename Appendix (blank to skip)
  kwi <- 4 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.pt$rfFilename <- paste(substr(prm.pt$trainFilename,1,(nchar(prm.pt$trainFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.pt$rfFilename,(nchar(prm.pt$rfFilename)-3),nchar(prm.pt$rfFilename))==".bin")) {
      stop('ERROR WITH PARAMETER FILE ... random forest model output filename must be of type ".bin"')
    }
  } else {
    prm.pt$rfFilename <- ''
  }
  
  # Program Output Log .txt Filename Appendix (blank to skip)
  kwi <- 5 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.pt$outputFilename <- paste(substr(prm.pt$trainFilename,1,(nchar(prm.pt$trainFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.pt$outputFilename,(nchar(prm.pt$outputFilename)-3),nchar(prm.pt$outputFilename))==".txt")) {
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.pt$outputFilename <- ''
  }
  
  # Sorted Predictor Importance .txt Filename Appendix (blank to skip)
  kwi <- 6 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.pt$importanceFilename <- paste(substr(prm.pt$trainFilename,1,(nchar(prm.pt$trainFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.pt$importanceFilename,(nchar(prm.pt$importanceFilename)-3),nchar(prm.pt$importanceFilename))==".txt")) {
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.pt$importanceFilename <- ''
  }
  
  # Date and Time Column Name
  kwi <- 7 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$timeColName <- text$v1[r]
  
  # Prediction Target Source Column Name
  kwi <- 8 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$targetSource <- text$v1[r]
  
  # Calculated Prediction Target Column Name
  kwi <- 9 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$target <- text$v1[r]
  
  # Target alarm cutoff value
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$targetAlarmCutoff <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Future prediction time span (minutes)
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$rowFutureSpan <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Past calculated predictor time span (minutes)
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$rowPastSpan <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Dominant observation time interval (seconds)
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$dominantTimeInterval <- as.numeric(text$v1[r])
    if (prm.pt$dominantTimeInterval>0) {
      prm.pt$rowFutureSpan <- prm.pt$rowFutureSpan * 60 / prm.pt$dominantTimeInterval
      prm.pt$rowPastSpan <- prm.pt$rowPastSpan * 60 / prm.pt$dominantTimeInterval
    }
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Number of observations used for training
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$trainSampleSize <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Balance training above/below alarm cutoff (Yes/No)
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$balanceTrainingSamples <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # ntree - number of Random Forest Trees
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$ntree <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # mtry - number of predictors per decision
  kwi <- 17
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$mtry <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # do.trace - diagnostic tree interval
  kwi <- 18
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.pt$do.trace <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }

  # Verbose log file output (Yes/No)
  kwi <- 19
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.pt$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Source Predictors from input file
  kwi <- 20 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+1
  if (text$count[r1] !=1 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket fields (i.e. bracket pair [])
                                        on line immediately following keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.pt$cpnames <- text$v1[r1]
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==1) {
    prm.pt$cpnames <- append(prm.pt$cpnames,text$v1[r1])
    r1 <- r1+1
  }
    
  # All Predictors for Training/Predicting
  kwi <- 21 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+1
  if (text$count[r1] !=1 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket fields (i.e. bracket pair [])
                                        on line immediately following keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.pt$tpnames <- text$v1[r1]
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==1) {
    prm.pt$tpnames <- append(prm.pt$tpnames,text$v1[r1])
    r1 <- r1+1
  }

  # prm.pt$nrowTrain <- 250000 # number of rows to load (-1 for all)

  
  return(prm.pt)
}