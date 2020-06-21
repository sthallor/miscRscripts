#######################################################################################
# get.prm.rt.R - read and decode ASCII parameters file for real time classifier
# Ensign Energy Services Inc. retains all rights to this software
# FHS, March 8, 2019
#######################################################################################

# Program Parameters ASCII text template
#
# Real Time Rig Classify V3.0 Program Parameters 3/8/2019
# 
# Title  [3/8/2019 using 5m lookahead model]
# 
# Training Parameters Filename                       [C:/Users/Fred.Seymour/RCode/master/parms/dev/classify/trainRT.prm]
# Data cleaning Parameters Filename (blank for none) [C:/Users/Fred.Seymour/RCode/master/parms/dev/classify/dcleanRT.prm]
# 
# Real Time ODBC Connect Name             [rig550]
# 
# Directory for output data [C:/Users/Fred.Seymour/EDR_Data/190204_RTclassify_tests/190221_error_testing/]
# Filename for log .txt output (blank for none)                     [log.txt]
# Filename for run to run summary log .csv output (blank for none)  [runlog.csv]
# Filename for results .csv output (blank for none)                 [results.csv]
# Filename for diagnostics .csv output (blank for none)             [diagnostics.csv]
#                                                                    
# Suspend time between checking for input file update (seconds)     [2]
# Elapsed time program duration (hours)                             [0.02]
# Real time lag delay (seconds)                                     [0]
# Tolerance between current and previous SQL query for diagnostic   [1e-6]
# Append time stamp to output filenames (Yes/No)                    [No]
# Early save for run log, proportion of elapsed time (1 for none)   [0.95] 
# Verbose log file output (Yes/No)                                  [No]
#                                                                    
# Output Time Epoch and rig_state code to database (Yes/No) [No]
# rig_state codes for sql output to database
# rig_state         code  
# [BackReaming]     [40]
# [Circulating]     [60]
# [Connecting]      [70]
# [Data_Incomplete] [0]
# [Error_In_Data]   [2]
# [Other]           [5]
# [RDrilling]       [90]
# [Reaming]         [50]
# [SDrilling]       [80]
# [TrippingIn]      [20]
# [TrippingOut]     [30]
# [TripInConnect]   [25]
# [TripOutConnect]  [35]
#                                                                    
# Perform Post Processing Rig State Consolidation (Yes/No)         [Yes]
# Post Processing Consolidation Tests
# csn=Current State Name, csem=Current State Max Elapsed Minutes
# bsn=Before State Name, bsdm=Before State Distance Minutes
# asn=After State Name, asdm=After State Distance Minutes
# csnn=Current State New Name
# csn              csem    bsn              bsdm  asn               asdm  csnn (Oct 28, 2018) 
# [Other]          [1.0]   [Connecting]     [0.1] [Connecting]      [0.1] [Connecting] 
# [Circulating]    [0.2]   [RDrilling]      [0.1] [RDrilling]       [0.1] [RDrilling] 

get.prm.rt <- function(prmFilename) {
  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  
  text <- prm.decode(text)
  
  keywords <- c('Real Time Rig Classify V3.0 Program Parameters 3/8/2019',
                'Title',
                'Training Parameters Filename',
                'Data cleaning Parameters Filename (blank for none)',
                'Real Time ODBC Connect Name',
                'Directory for output data',
                'Filename for log .txt output (blank for none)',
                'Filename for run to run summary log .csv output',
                'Filename for results .csv output (blank for none)',
                'Filename for diagnostics .csv output (blank for none)',
                'Suspend time between checking for input file update (seconds)',
                'Elapsed time program duration (hours)',
                'Real time lag delay (seconds)',
                'Tolerance between current and previous SQL query for diagnostic',
                'Append time stamp to output filenames (Yes/No)',
                'Early save for run log, proportion of elapsed time',
                'Verbose log file output (Yes/No)',
                'Output Time Epoch and rig_state code to database (Yes/No)',
                'rig_state codes for sql output to database',
                'Perform Post Processing Rig State Consolidation',
                'Post Processing Consolidation Tests')
  
  # Initialize parameters list
  prm.rt <- list()
  # copies text parameters file verbatim
  prm.rt$text <- text$text
  
  # Real Time Rig Classify V3.0 Program Parameters 8/24/2017
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.rt$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$title <- text$v1[r]
  
  # Training Parameters Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi])) 
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$trainPrmFilename <- text$v1[r]
  if(!(substr(prm.rt$trainPrmFilename,(nchar(prm.rt$trainPrmFilename)-3),nchar(prm.rt$trainPrmFilename))==".prm")) {
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
    prm.rt$dcleanPrmFilename <- text$v1[r]
    if(!(substr(prm.rt$dcleanPrmFilename,(nchar(prm.rt$dcleanPrmFilename)-3),nchar(prm.rt$dcleanPrmFilename))==".prm")) {
      cat('\nERROR with dclean parameters.. data clean parameters filename must be of type ".prm"\n',prm.rt$dcleanPrmFilename)
      stop('ERROR WITH PARAMETER FILE ... dclean parameters filename must end with ".prm"')
    }
  } else { # if blank name then file not written
    prm.rt$dcleanPrmFilename <- ''
  }

  # Real Time ODBC Connect Name
  kwi <- 5
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$ODBCConnectName <- text$v1[r]
  
  # Directory for output data
  kwi <- 6
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rt$outputDirectory <- text$v1[r]
    if (substr(prm.rt$outputDirectory,nchar(prm.rt$outputDirectory),nchar(prm.rt$outputDirectory)) != "/")
      prm.rt$outputDirectory <- paste0(prm.rt$outputDirectory,"/")
  } else {
    prm.rt$outputDirectory <- ""
  }
  
  # Filename for log .txt output (blank for none)
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rt$outputLogFilename <- paste0(prm.rt$outputDirectory,text$v1[r])
    if(!(substr(prm.rt$outputLogFilename,(nchar(prm.rt$outputLogFilename)-3),nchar(prm.rt$outputLogFilename))==".txt")) {
      cat('\nERROR with classify parameters.. program output log filename must be of type ".txt"\n',prm.rt$outputLogFilename)
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rt$outputLogFilename <- ''
  }
  
  # Filename for run to run summary log .csv output
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rt$outputRunLogFilename <- paste0(prm.rt$outputDirectory,text$v1[r])
    if(!(substr(prm.rt$outputRunLogFilename,(nchar(prm.rt$outputRunLogFilename)-3),nchar(prm.rt$outputRunLogFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output run to run summary log filename must be of type ".csv"\n',prm.rt$outputRunLogFilename)
      stop('ERROR WITH PARAMETER FILE ... program run to run summary log output filename must be of type ".csv"')
    }
  } else { # if blank then output filename is blank and output is sent to default device or not saved
    prm.rt$outputRunLogFilename <- ''
  }
  
  # Filename for results .csv output (blank for none)
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rt$outputResultFilename <- paste0(prm.rt$outputDirectory,text$v1[r])
    if(!(substr(prm.rt$outputResultFilename,(nchar(prm.rt$outputResultFilename)-3),nchar(prm.rt$outputResultFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output results filename must be of type ".csv"\n',prm.rt$outputResultFilename)
      stop('ERROR WITH PARAMETER FILE ... program results output filename must be of type ".csv"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rt$outputResultFilename <- ''
  }
  
  # Filename for diagnostic .csv output (blank for none)
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rt$outputDiagFilename <- paste0(prm.rt$outputDirectory,text$v1[r])
    if(!(substr(prm.rt$outputDiagFilename,(nchar(prm.rt$outputDiagFilename)-3),nchar(prm.rt$outputDiagFilename))==".csv")) {
      cat('\nERROR with classify parameters.. program output diagnostic filename must be of type ".csv"\n',prm.rt$outputDiagFilename)
      stop('ERROR WITH PARAMETER FILE ... program diagnostic output filename must be of type ".csv"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rt$outputDiagFilename <- ''
  }

  # Suspend time between checking for input file update (seconds)
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rt$sleeptime <- as.numeric(text$v1[r])  
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
    prm.rt$elapsedtime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Real time lag delay (seconds)
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rt$lagDelay <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Tolerance between current and previous SQL query for diagnostic
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rt$matchTolerance <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  
  

  # Append time stamp to output filenames (Yes/No)
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$appendTimestamp <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  if (prm.rt$appendTimestamp==T) {
    # Create time stamp YYMMDDHHMMSS and insert into output filenames
    timeStamp <- as.character(Sys.time())
    timeStamp <- paste0(substr(timeStamp,3,4),substr(timeStamp,6,7),substr(timeStamp,9,10),
                        substr(timeStamp,12,13),substr(timeStamp,15,16),substr(timeStamp,18,19))
    if (nchar(prm.rt$outputLogFilename)>0) {
      prm.rt$outputLogFilename <- paste0(substr(prm.rt$outputLogFilename,1,(nchar(prm.rt$outputLogFilename)-4)),
                                         '_',timeStamp,'.txt')
    }
    if (nchar(prm.rt$outputResultFilename)>0) {
      prm.rt$outputResultFilename <- paste0(substr(prm.rt$outputResultFilename,1,(nchar(prm.rt$outputResultFilename)-4)),
                                         '_',timeStamp,'.csv')
    }
    if (nchar(prm.rt$outputDiagFilename)>0) {
      prm.rt$outputDiagFilename <- paste0(substr(prm.rt$outputDiagFilename,1,(nchar(prm.rt$outputDiagFilename)-4)),
                                            '_',timeStamp,'.csv')
    }
  }
  
  # Early save for run log, proportion of elapsed time
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rt$earlySaveProportion <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  if (prm.rt$earlySaveProportion<0) prm.rt$earlySaveProportion <- 0
  if (prm.rt$earlySaveProportion>1) prm.rt$earlySaveProportion <- 1
  prm.rt$earlySaveFlag <- FALSE  # Indicate whether early save has already occured
  
   # Verbose log file output (Yes/No)
  kwi <- 17
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Output Time Epoch and rig_state code to database (Yes/No)
  kwi <- 18
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$sqlWriteRigStateCode <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # rig_state codes for sql output to database
  kwi <- 19 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+2
  if (text$count[r1] !=2 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 2 bracket fields (i.e. bracket pair [])
                                        on line 2 lines below following keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.rt$rigStateCode <- data.frame(rig_state=text$v1[r1],code=text$v2[r1])
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==2) {
    prm.rt$rigStateCode <- rbind(prm.rt$rigStateCode,data.frame(rig_state=text$v1[r1],code=text$v2[r1]))
    r1 <- r1+1
  }
  prm.rt$rigStateCode$rig_state <- as.character(prm.rt$rigStateCode$rig_state)
  prm.rt$rigStateCode$code <- as.character(prm.rt$rigStateCode$code)
  
  # Perform Post Processing Rig State Consolidation
  kwi <- 20
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rt$ppflag <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Post Processing Consolidation Tests
  if (prm.rt$ppflag) {
    kwi <- 21
    r <- pmatch(keywords[kwi],text$text)
    if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                               MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
    r1 <- r+6
    if (text$count[r1] !=7 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 7 bracket fields (i.e. bracket pair [])
                                          on line 6 lines below keywords "%s"\n%s\n', 
                                          keywords[kwi],text$text[r1]))
    prm.rt$pptest <- data.frame(csn=as.character(text$v1[r1]),csem=as.numeric(text$v2[r1]),
                                bsn=as.character(text$v3[r1]),bsdm=as.numeric(text$v4[r1]),
                                asn=as.character(text$v5[r1]),asdm=as.numeric(text$v6[r1]),
                                csnn=as.character(text$v7[r1]))
    r1 <- r1+1
    while(r1 <= nrow(text) & text$count[r1]==7) {
      prm.rt$pptest <- rbind(prm.rt$pptest,
                             data.frame(csn=as.character(text$v1[r1]),csem=as.numeric(text$v2[r1]),
                                        bsn=as.character(text$v3[r1]),bsdm=as.numeric(text$v4[r1]),
                                        asn=as.character(text$v5[r1]),asdm=as.numeric(text$v6[r1]),
                                        csnn=as.character(text$v7[r1])))
      r1 <- r1+1
    }
    prm.rt$pptest$csn <- as.character(prm.rt$pptest$csn)
    prm.rt$pptest$bsn <- as.character(prm.rt$pptest$bsn)
    prm.rt$pptest$asn <- as.character(prm.rt$pptest$asn)
    prm.rt$pptest$csnn <- as.character(prm.rt$pptest$csnn)
  }
  
  return(prm.rt)
}