#######################################################################################
# get.prm.dc.R - read and decode data clean ASCII parameters file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 9, 2019
#######################################################################################
# Sample parameters file is listed below
# 
# Program Data Clean V3.0 2/15/2017
# 
# Title  [EDR 10/24/2016]
# 
# Input Filenames (one file per line) for data cleaning
# [/Users/Fred.Seymour/EDR_Data/151217_Datasets/train/DR155_training_rig_states_V6.csv]
# 
# Clean Data Output Filename Appendix  [_clean.csv]
# Program Output Log Filename Appendix [_output.txt]
#
# Input file is raw Historian data needing reshape (Yes/No)   [No]
# Time Column Name                                            [EDR_DateTime]
# Average values over constant time interval (Yes/No)         [No]
# Constant time interval in seconds                           [10]
# Constant time interval offset in seconds                    [0]
# Max timespan (sec) for inserting missing observations       [600]
# Convert to UTC timezone if UTC offset available (Yes/No)    [No]
# Verbose logfile output (Yes/No)                             [Yes]
# 
# Save all columns (Yes=all, No=save only selected columns)   [Yes]
# Global numeric value or code indicating missing value (NA)  [-999.25]
# Global numeric value below which set to missing (NA)        [-999]
# Global numeric value above which set to missing (NA)        [100000]
# Min number of unique values to be considered continuous     [6]
# Min data density to be considered continuous                [0.05]
# Max timespan (sec) for interpolating continuous variables   [600]
# 
# Non-standard predictor variable name conversion to standard names
# Option to overwrite standard predictor variable name if it already exists
# Linear transformation calculation: standard = (Non-standard - Intercept)*Slope
# Non-standard name    Standard name               Overwrite Intercept Slope
# [EDR_BitPosition]    [EDR_BitDepth]              [Yes]     [0]       [1]
# [EDR_TopDriveRPM]    [EDR_RotaryRPM]             [Yes]     [0]       [1] 
# 
# Convert Units Of Measure (UOM) to Standard Units (Yes/No)       [Yes]
# UOMType inserted into reshaped UOM column name(s) with historian data
# DColName           UOMColName    UOMType   InputUnit OutputUnit Intercept Slope
# [EDR_BlockHeight]  [length_UOM]  [length]  [feet]    [meters]   [0]       [0.3048] 
# [EDR_BitDepth]     [length_UOM]  [length]  [feet]    [meters]   [0]       [0.3048]
# [EDR_HoleDepth]    [length_UOM]  [length]  [feet]    [meters]   [0]       [0.3048]
# [EDR_ROP]          [length_UOM]  [length]  [feet]    [meters]   [0]       [0.3048]
# 
# Selected columns for continuous variable cleaning parameters
# Column Name, Min=values below this set to NA, Floor=values below this set to floor
# Ceiling=values above this set to ceiling, Max=values above this set to NA
# MaxTime=max timespan for interpolating/extrapolating values
# NANumber=replace all NA values with numeric value listed (NA for none)
# Column Name                Min       Floor      Ceiling    Max      MaxTimespan NANumber
# [EDR_BlockHeight]          [-100]    [-25]      [1000]     [1000]   [600]       [NA]
# [EDR_BitDepth]             [-100]    [0]        [100000]   [100000] [600]       [NA]  
# [EDR_HoleDepth]            [-100]    [0]        [100000]   [100000] [600]       [NA]
# 
# Selected columns for discrete variable cleaning parameters
# [ensign_ac_rig/console/dq/tilt_rev]
# [ensign_ac_rig/dw/dq/crownsaver_cnsl_reset]
# [ensign_ac_rig/sect_dh1/read/radfan1_cntrlsrc]

get.prm.dc <- function(prmFilename) {
  # Read and decode text parameters file
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  
  keywords <- c('Program Data Clean V3.0 2/15/2017',
                'Title',
                'Input Filenames (one file per line) for data cleaning',
                'Clean Data Output Filename Appendix',
                'Program Output Log Filename Appendix',
                'Input file is raw Historian data needing reshape',
                'Time Column Name',
                'Average values over constant time interval',
                'Constant time interval in seconds',
                'Constant time interval offset in seconds',
                'Max timespan (sec) for inserting missing observations',
                'Convert to UTC timezone if UTC offset available (Yes/No)',
                'Verbose logfile output (Yes/No)',
                'Save all columns (Yes=all, No=save only selected columns)',
                'Global numeric value or code indicating missing value (NA)',
                'Global numeric value below which set to missing (NA)',
                'Global numeric value above which set to missing (NA)',
                'Min number of unique values to be considered continuous',
                'Min data density to be considered continuous',
                'Max timespan (sec) for interpolating continuous variables',
                'Non-standard predictor variable name conversion to standard names',
                'Convert Units Of Measure (UOM) to Standard Units',
                'Selected columns for continuous variable cleaning parameters',
                'Selected columns for discrete variable cleaning parameters')
  
  # Initialize parameters list
  prm.dc <- list()
  # copies text parameters file verbatim
  prm.dc$text <- text$text

  # Program Data Clean V1.0 2/18/2016
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.dc$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$title <- text$v1[r]
  
  # Input Filenames (one file per line) for data cleaning
  kwi <- 3
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+1
  if (text$count[r1] !=1 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket fields (i.e. bracket pair [])
                                        on line immediately following keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.dc$inputNames <- text$v1[r1]
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==1) {
    if(!(substr(text$v1[r1],(nchar(text$v1[r1])-3),nchar(text$v1[r1]))==".csv")) {
      stop('ERROR WITH PARAMETER FILE ... data clean input file name must be of type .csv')
    }
    prm.dc$inputNames <- append(prm.dc$inputNames,text$v1[r1])
    r1 <- r1+1
  }
  
  # Clean Data Output Filename Appendix
  kwi <- 4
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".csv")) 
    stop('ERROR WITH PARAMETER FILE ... data clean output file name appendix must be of type .csv')
  prm.dc$outputNames <- paste(substr(prm.dc$inputNames,1,(nchar(prm.dc$inputNames)-4)),text$v1[r],sep="")
  
  # Program Output Log Filename Appendix
  kwi <- 5
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".txt")) 
      stop('ERROR WITH PARAMETER FILE ... data clean output file name appendix must be of type .txt')
    prm.dc$logNames <- paste(substr(prm.dc$inputNames,1,(nchar(prm.dc$inputNames)-4)),text$v1[r],sep="")
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.dc$logNames <- rep('',length(prm.dc$inputNames))
  }

  # Input file is raw Historian data needing reshape
  kwi <- 6
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$reshapeHistorianData <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Time Column Name
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$timeColName <- text$v1[r]
  
  # Average values over constant time interval
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$timeAvg <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Constant time interval in seconds
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$timeInterval <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Constant time interval offset in seconds
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$timeOffset <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Max timespan (sec) for inserting missing observations
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$insertMaxTimespan <- as.numeric(text$v1[r])
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }

  # Convert to UTC timezone if UTC offset available
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$UTCOffset <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Verbose logfile output (Yes/No)
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Save all columns (Yes=all, No=save only selected columns)
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$saveAll <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Global numeric value or code indicating missing value (NA)
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$globalNumberCodeNA <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Global numeric value below which set to missing (NA)
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$globalMinNA <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Global numeric value above which set to missing (NA)
  kwi <- 17
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$globalMaxNA <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Min number of unique values to be considered continuous
  kwi <- 18
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$contMinUnique <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Min data density to be considered continuous
  kwi <- 19
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$contMinDensity <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Max timespan (sec) for interpolating continuous variables
  kwi <- 20
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.dc$contMaxTimespan <- as.numeric(text$v1[r])
    prm.dc$contMaxLook <- as.numeric(text$v1[r])/(prm.dc$timeInterval*2)
    prm.dc$discreteMaxLook <- prm.dc$contMaxLook  # Set discrete max look = cont max look, FHS Mar 9, 2019
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Non-standard predictor variable name conversion to standard names
  kwi <- 21
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+3
  if (text$count[r1] !=3 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 3 bracket fields (i.e. bracket pair [])
                                        on line 3 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.dc$namefixp <- data.frame(nonStandardName=as.character(text$v1[r1]),
                               standardName=as.character(text$v2[r1]),
                               overwrite=if(toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE)
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==3) {
    prm.dc$namefixp <- rbind(prm.dc$namefixp,data.frame(nonStandardName=as.character(text$v1[r1]),
                                                      standardName=as.character(text$v2[r1]),
                                                      overwrite=if(toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE))
    r1 <- r1+1
  }
  prm.dc$namefixp$nonStandardName <- as.character(prm.dc$namefixp$nonStandardName)
  prm.dc$namefixp$standardName <- as.character(prm.dc$namefixp$standardName)
  
  # Convert Units Of Measure (UOM) to Standard Units (Yes/No)
  kwi <- 22
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.dc$convertUOM <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  r1 <- r+3
  if (text$count[r1] !=6 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 6 bracket fields (i.e. bracket pair [])
                                        on line 3 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.dc$standardUOM <- data.frame(dColName=as.character(text$v1[r1]),
                                uomColName=as.character(text$v2[r1]),
                                uomType=as.character(text$v3[r1]),
                                inputUnit=as.character(text$v4[r1]),
                                outputUnit=as.character(text$v5[r1]),
                                formula=as.character(text$v6[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==6) {
    prm.dc$standardUOM <- rbind(prm.dc$standardUOM,data.frame(dColName=as.character(text$v1[r1]),
                                                              uomColName=as.character(text$v2[r1]),
                                                              uomType=as.character(text$v3[r1]),
                                                              inputUnit=as.character(text$v4[r1]),
                                                              outputUnit=as.character(text$v5[r1]),
                                                              formula=as.character(text$v6[r1])))
    r1 <- r1+1
  }
  prm.dc$standardUOM$dColName <- as.character(prm.dc$standardUOM$dColName)
  prm.dc$standardUOM$uomColName <- as.character(prm.dc$standardUOM$uomColName)
  prm.dc$standardUOM$uomType <- as.character(prm.dc$standardUOM$uomType)
  prm.dc$standardUOM$inputUnit <- as.character(prm.dc$standardUOM$inputUnit)
  prm.dc$standardUOM$outputUnit <- as.character(prm.dc$standardUOM$outputUnit)
  prm.dc$standardUOM$formula <- as.character(prm.dc$standardUOM$formula)

    # Selected columns for continuous variable cleaning parameters
  kwi <- 23
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+6
  if (text$count[r1] !=7 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 7 bracket fields (i.e. bracket pair [])
                                        on line 6 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.dc$valueLimits <- data.frame(name=as.character(text$v1[r1]),
                                   minNA=if(is.na(as.numeric(text$v2[r1]))) 0 else as.numeric(text$v2[r1]),
                                   floor=if(is.na(as.numeric(text$v3[r1]))) 0 else as.numeric(text$v3[r1]),
                                   ceiling=if(is.na(as.numeric(text$v4[r1]))) 0 else as.numeric(text$v4[r1]),
                                   maxNA=if(is.na(as.numeric(text$v5[r1]))) 0 else as.numeric(text$v5[r1]),
                                   timespan=if(is.na(as.numeric(text$v6[r1]))) 0 else as.numeric(text$v6[r1]),
                                   maxLook=if(is.na(as.numeric(text$v6[r1]))) 0 else round(as.numeric(text$v6[r1])/
                                     (prm.dc$timeInterval*2)),
                                   NANum=text$v7[r1])
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==7) {
    prm.dc$valueLimits <- rbind(prm.dc$valueLimits,data.frame(name=as.character(text$v1[r1]),
                                                        minNA=if(is.na(as.numeric(text$v2[r1]))) 0 else as.numeric(text$v2[r1]),
                                                        floor=if(is.na(as.numeric(text$v3[r1]))) 0 else as.numeric(text$v3[r1]),
                                                        ceiling=if(is.na(as.numeric(text$v4[r1]))) 0 else as.numeric(text$v4[r1]),
                                                        maxNA=if(is.na(as.numeric(text$v5[r1]))) 0 else as.numeric(text$v5[r1]),
                                                        timespan=if(is.na(as.numeric(text$v6[r1]))) 0 else as.numeric(text$v6[r1]),
                                                        maxLook=if(is.na(as.numeric(text$v6[r1]))) 0 else round(as.numeric(text$v6[r1])/
                                                          (prm.dc$timeInterval*2)),
                                                        NANum=text$v7[r1]))
    r1 <- r1+1
  }
  prm.dc$valueLimits$name <- as.character(prm.dc$valueLimits$name)
  prm.dc$valueLimits$NANum <- as.character(prm.dc$valueLimits$NANum)
  
  # Selected columns for discrete variable cleaning parameters
  kwi <- 24
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+1
  if (text$count[r1] !=1 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket fields (i.e. bracket pair [])
                                        on line 1 row below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.dc$discrete <- data.frame(name=as.character(text$v1[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==1) {
    prm.dc$discrete <- rbind(prm.dc$discrete,data.frame(name=as.character(text$v1[r1])))
    r1 <- r1+1
  }
  prm.dc$discrete$name <- as.character(prm.dc$discrete$name)
  
  return(prm.dc)
}
