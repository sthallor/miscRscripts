#######################################################################################
# get.prm.predict.R - read and decode historian prediction ASCII parameters file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Apr 26, 2017
#######################################################################################

# Program Parameters ASCII text template  
# Program Predict V3.0 4/26/2017
#   
# Title  [Random Forest High Value Alarm Predictions 4/04/2017]
#   
# Predict Training Parameters Filename [C:/Users/Fred.Seymour/Rcode/master/parms/dev/predict/predictTrain.prm]
#   
# Select prediction data filename by uncommenting (remove pound sign) on one line
# # Data Filename       [C:/Users/Fred.Seymour/Historian_Data/170222_MachineHealth/170222_DR156_IGBT_temp_All2016_clean.csv]
# Data Filename       [C:/Users/Fred.Seymour/Rcode/TestData/predict/DR156_IGBT_temp_clean.csv]
#   
# Program Output Log .txt Filename Appendix       (blank to skip) []
# Plot Output .pdf Filename Appendix              (blank to skip) [_plot.pdf]
# Diagnostics Output .csv Filename Appendix       (blank to skip) [_diagnostic.csv]
#   
# Number of rows to load (-1 for all)      [50000]
# Prediction Processing chunk size         [10000]
# Minimum high events count for histogram  [1]
# Positive Predictive Value f_score factor [1]
# Strip Chart interval (0 to skip)         [10000]
  
get.prm.predict <- function(prmFilename) {  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  keywords <- c('Program Predict V3.0 4/26/2017',
                'Title',
                'Predict Training Parameters Filename',
                'Data Filename',
                'Program Output Log .txt Filename Appendix',
                'Plot Output .pdf Filename Appendix',
                'Diagnostics Output .csv Filename Appendix',
                'Number of rows to load (-1 for all)',
                'Prediction Processing chunk size',
                'Minimum high events count for histogram',
                'Positive Predictive Value f_score factor',
                'Strip Chart interval (0 to skip)')
  
  # Initialize parameters list
  prm.p <- list()
  # copies text parameters file verbatim
  prm.p$text <- text$text
  
  # Program Historian Predict Train V3.0 3/14/2017
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.p$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.p$title <- text$v1[r]
  
  # Predict Training Parameters Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.p$trainPrmFilename <- text$v1[r]
  if(!(substr(prm.p$trainPrmFilename,(nchar(prm.p$trainPrmFilename)-3),nchar(prm.p$trainPrmFilename))==".prm")) {
    stop('ERROR WITH PARAMETER FILE ... training parameters file name must be of type ".prm"')
  }
  
  # Data Filename
  kwi <- 4 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.p$predictFilename <- text$v1[r]
  if(!(substr(prm.p$predictFilename,(nchar(prm.p$predictFilename)-3),nchar(prm.p$predictFilename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... training file must be of type ".csv"')
  }
  
  # Program Output Log .txt Filename Appendix
  kwi <- 5 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.p$outputFilename <- paste(substr(prm.p$predictFilename,1,(nchar(prm.p$predictFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.p$outputFilename,(nchar(prm.p$outputFilename)-3),nchar(prm.p$outputFilename))==".txt")) {
      stop('ERROR WITH PARAMETER FILE ... program Log output filename must be of type ".txt"')
    }
  } else {
    prm.p$outputFilename <- ''
  }
  
  # Plot output .pdf Filename Appendix
  kwi <- 6 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.p$plotFilename <- paste(substr(prm.p$predictFilename,1,(nchar(prm.p$predictFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.p$plotFilename,(nchar(prm.p$plotFilename)-3),nchar(prm.p$plotFilename))==".pdf")) {
      stop('ERROR WITH PARAMETER FILE ... chart filename must be of type ".pdf"')
    }
  } else {
    prm.p$plotFilename <- ''
  }
  
  # Diagnostics Output .csv Filename Appendix
  kwi <- 7 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.p$diagFilename <- paste(substr(prm.p$predictFilename,1,(nchar(prm.p$predictFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.p$diagFilename,(nchar(prm.p$diagFilename)-3),nchar(prm.p$diagFilename))==".csv")) {
      stop('ERROR WITH PARAMETER FILE ... diagnostic filename must be of type ".csv"')
    }
  } else {
    prm.p$diagFilename <- ''
  }
  
    # Number of rows to load (-1 for all)
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.p$nrowPredict <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Prediction Processing chunk size
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.p$predictChunkSize <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Minimum high events count for histogram
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.p$highCountForHist <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Positive Predictive Value f_score factor
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.p$ppvFscoreFactor <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Strip Chart interval (0 to skip)
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.p$stripchartInterval <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  return(prm.p)
}