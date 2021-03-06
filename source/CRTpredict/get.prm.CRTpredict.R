#######################################################################################
# get.prm.RTpredict.R - read and decode ASCII parameters file for real time predictions
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 12, 2017
#######################################################################################

# Program Parameters ASCII text template
#
# Flat File Handshake Real Time Rig Predict V3.0 Program Parameters 12/12/2017
# 
# Title  [12/12/2017]
# 
# Training Parameters Filename                       [C:/Users/Fred.Seymour/RCode/master/parms/dev/predict/predictTrain.prm]
# Data cleaning Parameters Filename (blank for none) [C:/Users/Fred.Seymour/RCode/master/parms/dev/predict/dcleanRT.prm]
# 
# Directory for output data [C:/Users/Fred.Seymour/Rcode/TestData/predict/]
# Filename for predictor input .csv data                [input.csv]
# Filename for predictor output .csv result             [output.csv]
# Filename for log .txt output (blank for none)         [_log.txt]
# Filename for results .csv output (blank for none)     [_results.csv]
# Filename for diagnostics .csv output (blank for none) [_diagnostics.csv]
# 
# Suspend time between checking for input file update (seconds)     [2]
# Elapsed time program duration (hours)                             [0.01]
# Alarm random forest tree count proportion cutoff                  [0.50]
# Append time stamp to output filenames (Yes/No)                    [No]
# Verbose log file output (Yes/No)                                  [No] 


get.prm.RTpredict <- function(prmFilename) {
  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  
  text <- prm.decode(text)
  
  keywords <- c('Flat File Handshake Real Time Rig Predict V3.0 Program Parameters 12/12/2017',
                'Title',
                'Training Parameters Filename',
                'Data cleaning Parameters Filename (blank for none)',
                'Directory for output data',
                'Filename for predictor input .csv data',
                'Filename for predictor output .csv result',
                'Filename for log .txt output (blank for none)',
                'Filename for results .csv output (blank for none)',
                'Filename for diagnostics .csv output (blank for none)',
                'Suspend time between checking for input file update (seconds)',
                'Elapsed time program duration (hours)',
                'Alarm random forest tree count proportion cutoff',
                'Append time stamp to output filenames (Yes/No)',
                'Verbose log file output (Yes/No)')
  
  # Initialize parameters list
  prm.rp <- list()
  # copies text parameters file verbatim
  prm.rp$text <- text$text
  
  # C callable Real Time Rig Predict V3.0 Program Parameters 12/10/2017
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.rp$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rp$title <- text$v1[r]
  
  # Training Parameters Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi])) 
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rp$trainPrmFilename <- text$v1[r]
  if(!(substr(prm.rp$trainPrmFilename,(nchar(prm.rp$trainPrmFilename)-3),nchar(prm.rp$trainPrmFilename))==".prm")) {
    stop('ERROR WITH PARAMETER FILE ... training parameter file must be of type .prm')
  }
  
  # Data cleaning Parameters Filename (blank for none)
  kwi <- 4
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$dcleanPrmFilename <- text$v1[r]
    if(!(substr(prm.rp$dcleanPrmFilename,(nchar(prm.rp$dcleanPrmFilename)-3),nchar(prm.rp$dcleanPrmFilename))==".prm")) {
      cat('\nERROR with dclean parameters.. data clean parameters filename must be of type ".prm"\n',prm.rp$dcleanPrmFilename)
      stop('ERROR WITH PARAMETER FILE ... dclean parameters filename must end with ".prm"')
    }
  } else { # if blank name then file not written
    prm.rp$dcleanPrmFilename <- ''
  }

  # Directory for output data
  kwi <- 5
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$outputDirectory <- text$v1[r]
    if (substr(prm.rp$outputDirectory,nchar(prm.rp$outputDirectory),nchar(prm.rp$outputDirectory)) != "/")
      prm.rp$outputDirectory <- paste0(prm.rp$outputDirectory,"/")
  } else {
    prm.rp$outputDirectory <- ""
  }
  
  # Filename for predictor input .csv data
  kwi <- 6
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$inputFilename <- paste0(prm.rp$outputDirectory,text$v1[r])
    if(!(substr(prm.rp$inputFilename,(nchar(prm.rp$inputFilename)-3),nchar(prm.rp$inputFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output log filename must be of type ".csv"\n',prm.rp$inputFilename)
      stop('ERROR WITH PARAMETER FILE ... program input filename must be of type ".csv"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rp$inputFilename <- ''
  }
  
  # Filename for predictor output .csv result
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$outputFilename <- paste0(prm.rp$outputDirectory,text$v1[r])
    if(!(substr(prm.rp$outputFilename,(nchar(prm.rp$outputFilename)-3),nchar(prm.rp$outputFilename))==".csv")) {
      cat('\nERROR with classify parameters.. prediction output filename must be of type ".csv"\n',prm.rp$outputFilename)
      stop('ERROR WITH PARAMETER FILE ... prediction output filename must be of type ".csv"')
    }
  } else { # if blank then output filename is blank and output is sent to default device
    prm.rp$outputFilename <- ''
  }
  
  
  
  
  
  # Filename for log .txt output (blank for none)
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$outputLogFilename <- paste0(prm.rp$outputDirectory,text$v1[r])
    if(!(substr(prm.rp$outputLogFilename,(nchar(prm.rp$outputLogFilename)-3),nchar(prm.rp$outputLogFilename))==".txt")) {
      cat('\nERROR with classify parameters.. program output log filename must be of type ".txt"\n',prm.rp$outputLogFilename)
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rp$outputLogFilename <- ''
  }
  
  # Filename for results .csv output (blank for none)
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$outputResultFilename <- paste0(prm.rp$outputDirectory,text$v1[r])
    if(!(substr(prm.rp$outputResultFilename,(nchar(prm.rp$outputResultFilename)-3),nchar(prm.rp$outputResultFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output log filename must be of type ".csv"\n',prm.rp$outputResultFilename)
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".csv"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rp$outputResultFilename <- ''
  }
  
  # Filename for diagnostic .csv output (blank for none)
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rp$outputDiagFilename <- paste0(prm.rp$outputDirectory,text$v1[r])
    if(!(substr(prm.rp$outputDiagFilename,(nchar(prm.rp$outputDiagFilename)-3),nchar(prm.rp$outputDiagFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output log filename must be of type ".csv"\n',prm.rp$outputDiagFilename)
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".csv"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rp$outputDiagFilename <- ''
  }

  # Suspend time between checking for input file update (seconds)
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rp$sleeptime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Elapsed time program duration (hours)
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rp$elapsedtime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Alarm random forest tree count proportion cutoff
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rp$rfGTCutoffAlarm <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }

  # Append time stamp to output filenames (Yes/No)
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rp$appendTimestamp <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  if (prm.rp$appendTimestamp==T) {
    # Create time stamp YYMMDDHHMMSS and insert into output filenames
    timeStamp <- as.character(Sys.time())
    timeStamp <- paste0(substr(timeStamp,3,4),substr(timeStamp,6,7),substr(timeStamp,9,10),
                        substr(timeStamp,12,13),substr(timeStamp,15,16),substr(timeStamp,18,19))
    if (nchar(prm.rp$outputLogFilename)>0) {
      prm.rp$outputLogFilename <- paste0(substr(prm.rp$outputLogFilename,1,(nchar(prm.rp$outputLogFilename)-4)),
                                         '_',timeStamp,'.txt')
    }
    if (nchar(prm.rp$outputResultFilename)>0) {
      prm.rp$outputResultFilename <- paste0(substr(prm.rp$outputResultFilename,1,(nchar(prm.rp$outputResultFilename)-4)),
                                         '_',timeStamp,'.csv')
    }
    if (nchar(prm.rp$outputDiagFilename)>0) {
      prm.rp$outputDiagFilename <- paste0(substr(prm.rp$outputDiagFilename,1,(nchar(prm.rp$outputDiagFilename)-4)),
                                            '_',timeStamp,'.csv')
    }
  }
  
   # Verbose log file output (Yes/No)
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rp$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  return(prm.rp)
}