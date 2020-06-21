#######################################################################################
# get.prm.rc.R - read and decode ASCII parameters file for rig state classifier
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Nov 8, 2018
#######################################################################################

# Program Parameters ASCII text template
#
# Rig Classify V3.0 Program Parameters 11/08/2018
# 
# Title  [11/08/2018]
# 
# Training Parameters Filename  [/Users/Fred.Seymour/RCode/master/parms/train.prm]
#
# Select drill rig filename by uncommenting (remove pound sign) on just one line
# Drill Rig .csv Input Filename [/Users/Fred.Seymour/EDR_Data/151130 Datasets/155/1-Chevron Midland AC-AF FEE 12 12HA - Rig 155-P.csv]
# 
# Drill Rig .csv Output Filename Appendix            (blank to skip) [_classify.csv]
# Drill Rig .csv Diagnostic Filename Appendix        (blank to skip) [_diagnostic.csv]
# Rig States Report .csv Filename Appendix           (blank to skip) [_rig_states.csv]
# Connecting Times Report .csv Filename Appendix     (blank to skip) [_connect_times.csv]
# Connecting Time Details .csv Filename Appendix     (blank to skip) [_connect_times_details.csv]
# Tripping Speed Report .csv Filename Appendix       (blank to skip) [_tripping_speeds.csv]
# Trip Connect Time Details .csv Filename Appendix   (blank to skip) [_trip_connect_times_details.csv]
# Program Output Log .txt Filename Appendix          (blank to skip) [_classify_log.txt]
# 
# Performance reporting parameters: Connect Times and Tripping Speeds
# Reporting by individual driller and by casing status (Yes/No)     [Yes]
# Maximum number of records for activity agglomeration              [120]
# Minimum delta hole depth for connect times report record (meters) [5]
# Connect Time ReamCC Time Threshold for separate record (minutes)  [15]
# Minimum StoS connect interval duration to be counted (minutes)    [1]
# Minimum active time for tripping speed report record (hours)      [0]
# Bit depth threshold for changing from delta block height to delta 
# bit depth in tripping speed active travel distance col (meters)   [300] 
# 
# Save individual rig state rf votes with diagnostic file (Yes/No)  [Yes]
# RFVote lower threshold for acceptable rig state classification    [0.5]
# Verbose log file output (Yes/No)                                  [Yes]
# Perform Post Processing Rig State Consolidation (Yes/No)          [Yes]
# 
# Post Processing Consolidation Tests
# csn=Current State Name, csem=Current State Max Elapsed Minutes
# bsn=Before State Name, bsdm=Before State Distance Minutes
# asn=After State Name, asdm=After State Distance Minutes
# csnn=Current State New Name
# csn           csem    bsn            bsdm  asn           asdm  csnn
# [Other]       [1.0]   [Connecting]   [0.1] [Connecting]  [0.1] [Connecting]
# [Other]       [1.0]   [TrippingIn]   [0.1] [TrippingIn]  [0.1] [TrippingIn]
# [Other]       [1.0]   [TrippingOut]  [0.1] [Trippingout] [0.1] [Trippingout]
# 
# Post Processing Changed Rig State Output Column Name     [rig_state_pp]
# Rig State Category Change Input Column Name              [DrillingCode]
# Rig_State     Change_Code            New_Rig_State
# [Other]       [1]                    [Rig Up]
# [Other]       [1A]                   [Move Rig]
# [Other]       [1B]                   [Rig up top drive]

get.prm.rc <- function(prmFilename) {
  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  
  text <- prm.decode(text)
  
  keywords <- c('Rig Classify V3.0 Program Parameters 11/08/2018',
                'Title',
                'Training Parameters Filename',
                'Drill Rig .csv Input Filename',
                'Drill Rig .csv Output Filename Appendix',
                'Drill Rig .csv Diagnostic Filename Appendix',
                'Rig States Report .csv Filename Appendix',
                'Connecting Times Report .csv Filename Appendix',
                'Connecting Time Details .csv Filename Appendix',
                'Tripping Speed Report .csv Filename Appendix',
                'Trip Connect Time Details .csv Filename Appendix',
                'Program Output Log .txt Filename Appendix',
                'Reporting by individual driller',
                'Maximum number of records for activity agglomeration',
                'Minimum delta hole depth for connect times report record',
                'Connect Time ReamCC Time Threshold for separate record (minutes)',
                'Minimum StoS connect interval duration to be counted (minutes)',
                'Minimum active time for tripping speed report record',
                'bit depth in tripping speed active travel distance col',
                'Save individual rig state rf votes with diagnostic file',
                'RFVote lower threshold for acceptable rig state classification',
                'Verbose log file output (Yes/No)',
                'Perform Post Processing Rig State Consolidation',
                'Post Processing Consolidation Tests',
                'Post Processing Changed Rig State Output Column Name',
                'Rig State Category Change Input Column Name')

  # Initialize parameters list
  prm.rc <- list()
  # copies text parameters file verbatim
  prm.rc$text <- text$text
  
  # Rig Classify V3.0 Program Parameters 11/08/2018
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.rc$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$title <- text$v1[r]
  
  # Training Parameters Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi])) 
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$trainPrmFilename <- text$v1[r]
  if(!(substr(prm.rc$trainPrmFilename,(nchar(prm.rc$trainPrmFilename)-3),nchar(prm.rc$trainPrmFilename))==".prm")) {
    stop('ERROR WITH PARAMETER FILE ... training parameter file must be of type .prm')
  }
  
  # Drill Rig .csv Input Filename
  kwi <- 4
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$rcInFilename <- text$v1[r]
  if(!(substr(prm.rc$rcInFilename,(nchar(prm.rc$rcInFilename)-3),nchar(prm.rc$rcInFilename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... input drill rig filename must end with ".csv"')
  }
  
  # Drill Rig .csv Output Filename Appendix
  kwi <- 5
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$rcOutFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$rcOutFilename,(nchar(prm.rc$rcOutFilename)-3),nchar(prm.rc$rcOutFilename))==".csv")) {
      cat('\nERROR with classify parameters.. rig states output filename must be of type ".csv"\n',prm.rc$rcOutFilename)
      stop('ERROR WITH PARAMETER FILE ... rig states output filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$rcOutFilename <- ''
  }

  # Drill Rig .csv Diagnostic Filename Appendix
  kwi <- 6
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$rcDiagFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$rcDiagFilename,(nchar(prm.rc$rcDiagFilename)-3),nchar(prm.rc$rcDiagFilename))==".csv")) {
      cat('\nERROR with classify parameters.. rig states diagnostic filename must be of type ".csv"\n',prm.rc$rcDiagFilename)
      stop('ERROR WITH PARAMETER FILE ... rig states diagnostic filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$rcDiagFilename <- ''
  }
    
  # Rig States Report .csv Filename Appendix
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$rcReportFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$rcReportFilename,(nchar(prm.rc$rcReportFilename)-3),nchar(prm.rc$rcReportFilename))==".csv")) {
      cat('\nERROR with classify parameters.. rig states report filename must be of type ".csv"\n',prm.rc$rcReportFilename)
      stop('ERROR WITH PARAMETER FILE ... rig states report filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$rcReportFilename <- ''
  }

  # Connecting Times Report .csv Filename Appendix
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$connReportFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$connReportFilename,(nchar(prm.rc$connReportFilename)-3),nchar(prm.rc$connReportFilename))==".csv")) {
      cat('\nERROR with classify parameters.. connecting times report filename must be of type ".csv"\n',prm.rc$connReportFilename)
      stop('ERROR WITH PARAMETER FILE ... connecting times report filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$connReportFilename <- ''
  }
  
  # Connecting Time Details .csv Filename Appendix
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$connDetailFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$connDetailFilename,(nchar(prm.rc$connDetailFilename)-3),nchar(prm.rc$connDetailFilename))==".csv")) {
      cat('\nERROR with classify parameters.. connecting time details report filename must be of type ".csv"\n',prm.rc$connDetailFilename)
      stop('ERROR WITH PARAMETER FILE ... connecting time details filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$connDetailFilename <- ''
  }
  
  # Tripping Speed Report .csv Filename Appendix
  kwi <- 10  
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$tripReportFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$tripReportFilename,(nchar(prm.rc$tripReportFilename)-3),nchar(prm.rc$tripReportFilename))==".csv")) {
      cat('\nERROR with classify parameters.. tripping speed report must be of type ".csv"\n',prm.rc$tripReportFilename)
      stop('ERROR WITH PARAMETER FILE ... tripping speed report filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$tripReportFilename <- ''
  }
  
  # Trip Connect Time Details .csv Filename Appendix
  kwi <- 11  
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$tripConnDetailFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$tripConnDetailFilename,(nchar(prm.rc$tripConnDetailFilename)-3),nchar(prm.rc$tripConnDetailFilename))==".csv")) {
      cat('\nERROR with classify parameters.. tripping connect time detail report must be of type ".csv"\n',prm.rc$tripConnDetailFilename)
      stop('ERROR WITH PARAMETER FILE ... tripping connect time detail report filename must end with ".csv"')
    }
  } else { # if blank name then file not written
    prm.rc$tripConnDetailFilename <- ''
  }
   
  # Program Output Log .txt Filename Appendix
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.rc$outputFilename <- paste(substr(prm.rc$rcInFilename,1,(nchar(prm.rc$rcInFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.rc$outputFilename,(nchar(prm.rc$outputFilename)-3),nchar(prm.rc$outputFilename))==".txt")) {
      cat('\nERROR with classify parameters.. program log output filename must be of type ".txt"\n',prm.rc$outputFilename)
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.rc$outputFilename <- ''
  }

  # Reporting by individual driller
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$singleDrillerReporting <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE

  # Maximum number of records for activity agglomeration
  kwi <- 14 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
    prm.rc$agglomerateCount <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Minimum delta hole depth for connect times report record
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$minDeltaHoleDepth <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Connect Time ReamCC Time Threshold for separate record (minutes)
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$ReamCCTime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Minimum StoS connect interval duration to be counted (minutes)
  kwi <- 17
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$minStoSConnectTime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Minimum active time for tripping speed report record
  kwi <- 18
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$minActiveTime <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # bit depth in tripping speed active travel distance col 
  kwi <- 19
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$bitDepthThreshold <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }

  # Save individual rig state rf votes with diagnostic file
  kwi <- 20
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$rfvAllSave <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # RFVote lower threshold for acceptable rig state classification
  kwi <- 21
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.rc$rfvotehreshold <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Verbose log file output (Yes/No)
  kwi <- 22
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Perform Post Processing Rig State Consolidation
  kwi <- 23
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$ppflag <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Post Processing Consolidation Tests
  if (prm.rc$ppflag) {
    kwi <- 24
    r <- pmatch(keywords[kwi],text$text)
    if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                               MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
    r1 <- r+6
    if (text$count[r1] !=7 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 7 bracket fields (i.e. bracket pair [])
                                          on line 6 lines below keywords "%s"\n%s\n', 
                                          keywords[kwi],text$text[r1]))
    prm.rc$pptest <- data.frame(csn=as.character(text$v1[r1]),csem=as.numeric(text$v2[r1]),
                                 bsn=as.character(text$v3[r1]),bsdm=as.numeric(text$v4[r1]),
                                 asn=as.character(text$v5[r1]),asdm=as.numeric(text$v6[r1]),
                                 csnn=as.character(text$v7[r1]))
    r1 <- r1+1
    while(r1 <= nrow(text) & text$count[r1]==7) {
      prm.rc$pptest <- rbind(prm.rc$pptest,
                              data.frame(csn=as.character(text$v1[r1]),csem=as.numeric(text$v2[r1]),
                                         bsn=as.character(text$v3[r1]),bsdm=as.numeric(text$v4[r1]),
                                         asn=as.character(text$v5[r1]),asdm=as.numeric(text$v6[r1]),
                                         csnn=as.character(text$v7[r1])))
      r1 <- r1+1
    }
    prm.rc$pptest$csn <- as.character(prm.rc$pptest$csn)
    prm.rc$pptest$bsn <- as.character(prm.rc$pptest$bsn)
    prm.rc$pptest$asn <- as.character(prm.rc$pptest$asn)
    prm.rc$pptest$csnn <- as.character(prm.rc$pptest$csnn)
  }
  
  # Post Processing Changed Rig State Output Column Name
  kwi <- 25 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$target_pp <- text$v1[r]
  
  # Rig State Category Change Input Column Name
  kwi <- 26 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.rc$pp_changeColname <- text$v1[r]
  r1 <- r+2
  if (text$count[r1] !=3 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 3 bracket fields (i.e. bracket pair [])
                                          on line 2 lines below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.rc$pp_changeRigState <- data.frame(oldRigState=as.character(text$v1[r1]),
                                         changeCode=as.character(text$v2[r1]),
                                         newRigState=as.character(text$v3[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==3) {
    prm.rc$pp_changeRigState <- rbind(prm.rc$pp_changeRigState,
                                      data.frame(oldRigState=as.character(text$v1[r1]),
                                                 changeCode=as.character(text$v2[r1]),
                                                 newRigState=as.character(text$v3[r1])))
    r1 <- r1+1
  }
  prm.rc$pp_changeRigState$oldRigState <- as.character(prm.rc$pp_changeRigState$oldRigState)
  prm.rc$pp_changeRigState$changeCode <- as.character(prm.rc$pp_changeRigState$changeCode)
  prm.rc$pp_changeRigState$newRigState <- as.character(prm.rc$pp_changeRigState$newRigState)
  
 return(prm.rc)
}