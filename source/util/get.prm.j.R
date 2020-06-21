#######################################################################################
# get.prm.j.R - read and decode ASCII parameters file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 15, 2017
#######################################################################################
#
# Program join V3.0 Program Parameters 2/15/2017
# 
# Title  [Historian to EDR time join 2/15/2017]
# 
# Input Historian .csv Filename   [/Users/Fred.Seymour/Rcode/testdata/offset/offset_Historian_Rig774_reshape_clean.csv]
# Input EDR .csv Filename         [/Users/Fred.Seymour/Rcode/testdata/offset/offset_EDR_Rig774_clean.csv]
# 
# Output joined .csv Filename     [/Users/Fred.Seymour/Rcode/testdata/offset/Historian_EDR_join_Rig774.csv]
# OUtput log .txt Filename        [/Users/Fred.Seymour/Rcode/testdata/offset/Historian_EDR_join_Log.txt]
# 
# Time offset to be applied to Historian time records before join   [350]

get.prm.j <- function(prmFilename) {
  # Read and decode text parameters file
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  
  keywords <- c('Program join V3.0 Program Parameters 2/15/2017',
                'Title',
                'Input Historian .csv Filename',
                'Input EDR .csv Filename',
                'Output joined .csv Filename',
                'Output log .txt Filename',
                'Time offset to be applied to Historian time records before join')
  
  # Initialize parameters list
  prm.j <- list()
  # copies text parameters file verbatim
  prm.j$text <- text$text
  
  # Program join V2.0 Program Parameters 3/03/2016
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.j$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.j$title <- text$v1[r]
  
  # Input Historian .csv Filename
  kwi <- 3 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.j$histName <- text$v1[r]
  if(!(substr(prm.j$histName,(nchar(prm.j$histName)-3),nchar(prm.j$histName))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... historian file must be of type .csv')
  }
  
  # Input EDR .csv Filename
  kwi <- 4 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.j$EDRName <- text$v1[r]
  if(!(substr(prm.j$EDRName,(nchar(prm.j$EDRName)-3),nchar(prm.j$EDRName))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... EDR file must be of type .csv')
  }
  
  # Output joined .csv Filename
  kwi <- 5 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".csv")) 
      stop('ERROR WITH PARAMETER FILE ... join output filename must be of type .csv')
    prm.j$joinName <- text$v1[r]
  } else { # if blank then output join filename is blank and no file is output
    prm.j$joinName <- ''
  }
  
  # Output log .txt Filename
  kwi <- 6 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".txt")) 
      stop('ERROR WITH PARAMETER FILE ... offset output log file name appendix must be of type .txt')
    prm.j$outputName <- text$v1[r]
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.j$outputName <- ''
  }
  
  # Time offset to be applied to Historian time records before join
  kwi <- 7
  r <- pmatch(keywords[kwi],text$text)
  prm.j$timeOffset <- 0 # default is no offset
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
      prm.j$timeOffset <- as.integer(text$v1[r])  
    } 
  }
  
  return(prm.j)
}