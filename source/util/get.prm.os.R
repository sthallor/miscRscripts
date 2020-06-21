#######################################################################################
# get.prm.os.R - read and decode ASCII parameters file for mainOffset.R
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 15, 2017
#######################################################################################
#
# Program Offset V3.0 Program Parameters 2/15/2017
# 
# Title  [Historian to EDR time offset 2/15/2017]
# 
# Historian .csv Filename         [/Users/Fred.Seymour/Historian_Data/160216_Datasets/BlockHeight_Rig774_WellID48543.csv]
# EDR .csv Filename               [/Users/Fred.Seymour/Historian_Data/160216_Datasets/input_EDR_file_output.csv]
# Output log .txt Filename        [/Users/Fred.Seymour/Historian_Data/160216_Datasets/histEDR_correlation.txt]
# Plot correlation .png Filename  [/Users/Fred.Seymour/Historian_Data/160216_Datasets/histEDR_correlation.png]
# 
# Historian Block Height Tag Name                           [BLOCK_HEIGHT]
# EDR Block Height Tag Name                                 [EDR_BlockHeight]
# Offset interval start (negative count from 0)             [-100]
# Offset interval stop (positive count from 0)              [100]

get.prm.os <- function(prmFilename) {
  # Read and decode text parameters file
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  
  keywords <- c('Program Offset V3.0 Program Parameters 2/15/2017',
                'Title',
                'Historian .csv Filename',
                'EDR .csv Filename',
                'Output log .txt Filename',
                'Plot correlation .png Filename',
                'Historian Block Height Tag Name',
                'EDR Block Height Tag Name',
                'Offset interval start',
                'Offset interval stop')
  
  # Initialize parameters list
  prm.os <- list()
  # copies text parameters file verbatim
  prm.os$text <- text$text
  
  # Program Data Clean V1.0 2/18/2016
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.os$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.os$title <- text$v1[r]
  
  # Historian .csv Filename
  kwi <- 3 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.os$histName <- text$v1[r]
  if(!(substr(prm.os$histName,(nchar(prm.os$histName)-3),nchar(prm.os$histName))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... historian file must be of type .csv')
  }
  
  # EDR .csv Filename
  kwi <- 4 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.os$EDRName <- text$v1[r]
  if(!(substr(prm.os$EDRName,(nchar(prm.os$EDRName)-3),nchar(prm.os$EDRName))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... EDR file must be of type .csv')
  }
  
  # Output log .txt Filename
  kwi <- 5 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".txt")) 
      stop('ERROR WITH PARAMETER FILE ... offset output log file name must be of type .txt')
    prm.os$outputName <- text$v1[r]
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.os$outputName <- ''
  }

  # Plot correlation .png Filename
  kwi <- 6 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".png")) 
      stop('ERROR WITH PARAMETER FILE ... offset plot filename must must be of type .png')
    prm.os$plotName <- text$v1[r]
  } else { # if blank then output plot filename is blank and output is sent to default device
    prm.os$plotName <- ''
  }
  
  # Historian Block Height Tag Name
  kwi <- 7 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.os$histKeyword <- text$v1[r]
  
  # EDR Block Height Tag Name
  kwi <- 8 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.os$EDRKeyword <- text$v1[r]
  
  # Offset interval start
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  prm.os$offsetMin <- (-60) # default is no number conversion to NA
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
      prm.os$offsetMin <- as.integer(text$v1[r])  
    } 
  }
  
  # Offset interval stop
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  prm.os$offsetMax <- 60 # default is no number conversion to NA
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
      prm.os$offsetMax <- as.integer(text$v1[r])  
    } 
  }
  
  return(prm.os)
}