#######################################################################################
# get.prm.predictStripchart.R - read and decode ASCII parameters file for prediction stripchart
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 10, 2017
#######################################################################################

# Program Parameters ASCII text template
#
# Program predictStripchart V3.0 4/27/2017
# 
# Title  [rig140]
# 
# Select drill rig filename by uncommenting (remove pound sign) on just one line
# Drill Rig .csv Input Filename [C:/Users/Fred.Seymour/Historian_Data/170407_IGBT_temp_predict/rig140/1702_clean_diagnostic.csv]
# 
# Stripchart Plot Output .pdf Filename Appendix (blank to skip) [_stripchart.pdf]
# 
# First observation number to plot                 [1]
# Last observation number to plot (use -1 for all) [-1]
# Number of observation for each strip chart       [5000]
# 
# Target Alarm cutoff                              [60]

get.prm.predictStripchart <- function(prmFilename) {
  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  
  text <- prm.decode(text)
  
  keywords <- c('Program predictStripchart V3.0 4/27/2017',
                'Title',
                'Drill Rig .csv Input Filename',
                'Stripchart Plot Output .pdf Filename Appendix',
                'First observation number to plot',
                'Last observation number to plot',
                'Number of observation for each strip chart',
                'Target Alarm cutoff')
  
  # Initialize parameters list
  prm.ps <- list()
  # copies text parameters file verbatim
  prm.ps$text <- text$text
  
  # Real Time Rig Predict V3.0 Program Parameters 4/26/2017
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.ps$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.ps$title <- text$v1[r]
  
  # Drill Rig .csv Input Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.ps$predictFilename <- text$v1[r]
  if(!(substr(prm.ps$predictFilename,(nchar(prm.ps$predictFilename)-3),nchar(prm.ps$predictFilename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... training file must be of type ".csv"')
  }
  
  # Stripchart Plot Output .pdf Filename Appendix
  kwi <- 4 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.ps$plotFilename <- paste(substr(prm.ps$predictFilename,1,(nchar(prm.ps$predictFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.ps$plotFilename,(nchar(prm.ps$plotFilename)-3),nchar(prm.ps$plotFilename))==".pdf")) {
      stop('ERROR WITH PARAMETER FILE ... strip chart plot filename must be of type ".pdf"')
    }
  } else {
    prm.ps$plotFilename <- ''
  }
  
  # First observation number to plot
  kwi <- 5
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.ps$from <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Last observation number to plot
  kwi <- 6
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.ps$to <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Number of observation for each strip chart
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.ps$interval <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Target alarm cutoff value
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.ps$targetAlarmCutoff <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  ######################################################
  # Some quickly stuffed in variables FHS May 9, 2017
  
  # Future prediction time span (minutes)
  prm.ps$rowFutureSpan <- 20
  # Dominant observation time interval (seconds)
  prm.ps$dominantTimeInterval <- 10
  
  prm.ps$rowFutureSpan <- prm.ps$rowFutureSpan * 60 / prm.ps$dominantTimeInterval
  
  # Positive Predictive Value f_score factor
  prm.ps$ppvFscoreFactor <- 1
  
  # Alarm random forest tree count proportion cutoff
  prm.ps$rfGTCutoffAlarm <- 0.43
  
  # Minimum high events count for histogram
  prm.ps$highCountForHist <- 1
  
  return(prm.ps)
}