#######################################################################################
# get.prm.t.R - read and decode classifier training ASCII parameters file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, March 27, 2019
#######################################################################################

# Program Parameters ASCII text template
#
# Program Classify Train V3.1 3/25/2019
# 
# Title  [Random Forest Training 3/25/2019]
# 
# Select training data filename by uncommenting (remove pound sign) on one line
# Training Data Filename       [/Users/Fred.Seymour/Rcode/testdata/classify2.1/DR155_training_rig_states_V6_clean.csv]
# # Training Data Filename       [/Users/Fred.Seymour/EDR_Data/160115_Datasets/train/160117_DR155_DR532_DR769_training_V1_clean.csv]
# 
# Random Forest Model .bin Filename Appendix                  [_randomForestModel.bin]
# Eigenvector Model .bin Filename Appendix                    [_eigenvectorModel.bin]
# Program Output Log .txt Filename Appendix (blank to skip)   [_randomForestLog.txt]
# Predictor Importance .txt Filename Appendix (blank to skip) [_randomForestImportance.txt]
# 
# Parameters for building calculated predictors
# Minimum number of observations for viable processing              [200]
# Date and Time column name                                         [time]
# Tolerance around zero for delta bit and hole depths               [1e-4]
# Maximum bit depth from hole bottom for drilling activity (meters) [10]
# Max delta block height rate for valid rig state (meters/s)        [3] 
# Max delta bit depth rate for valid rig state (meters/s)           [3]
# Max delta hole depth rate for valid rig state (meters/s)          [0.3]
# Max negative delta hole depth for valid rig state (meters)        [-2]
# 
# Target Column Name                         [rig_state]
# Number of Random Forest trees per Chunk    [25]
# Number of Random Forest chunks in run      [2]
# Number of trees per rf diagnostic trace    [5]
# Verbose log file output (Yes/No)           [Yes]
# 
# Double training dataset to model missing EDR_RotaryTorque (Yes/No) [Yes] 
# Numeric value to substitute for missing EDR_RotaryTorque values    [-1e6]
# 
# Convert Units Of Measure (UOM) to Standard Units for random forest model
# Calculation: OuputUnit = (InputUnit - Intercept)*Slope
# DColName           UOMColName    InputUnit OutputUnit Intercept Slope
# [EDR_BlockHeight]  [length_uom]  [feet]    [meters]   [0]       [0.3048] 
# 
# Number of records used before current record for calculated predictors [30]
# Number of records used after current record for calculated predictors  [30]
# 
# Specialty predictors - enter blank name to skip                 name
# Predictor - EDR_BitDepth to EDR_HoleDepth ratio                 [bitHoleDepthRatio]
# Predictor - EDR_BitDepth speed                                  [bitSpeed]
# Predictor - EDR_BitDepth distance from EDR_HoleDepth            [bitDistFromHoleBottom]
# Predictor - EDR_BitDepth delta from smoothed bit depth          [deltaBitDepthSmooth]
# Predictor - EDR_HoleDepth delta from smoothed hole depth        [deltaHoleDepthSmooth]
# Predictor - absolute EDR_BlockHeight travel distance            [deltaBlockHeightAbs]
# Predictor - consecutive constant EDR_BlockHeight interval count [blockHeightRunLengths]
# Predictor - EDR_ToolFace runmean of abs delta value             [toolFaceAbsDeltaRunMean]
# 
# Incoming file based predictors for building calculated predictors
# name         = incoming file column name for predictor
# base         = use incoming value as predictor (Yes/No)
# runmean      = runing mean calculated predictor (Yes/No)
# runsd        = runing standard deviation calculated predictor (Yes/No)
# delta        = delta from previous record calculated predictor (Yes/No)
# maxdeltarate = maximum rate of change per second for valid smoothing
# percent      = value percentile between min and max in the before to after range (Yes/No)
# eigencount   = number of eigenvector transformed calculated predictor inputs (0 for none)
# name                     base     runmean  runsd   delta  maxdeltarate  percent  eigencount                      
# [EDR_BlockHeight]        [Yes]    [Yes]    [Yes]   [Yes]  [3]           [Yes]    [10]
# [EDR_BitDepth]           [Yes]    [Yes]    [Yes]   [Yes]  [3]           [No]     [1]
# [EDR_HoleDepth]          [Yes]    [Yes]    [Yes]   [Yes]  [0.3]         [No]     [1]

get.prm.t <- function(prmFilename) {
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)

  text <- prm.decode(text)
  keywords <- c('Program Classify Train V3.1 3/27/2019',
                'Title',
                'Training Data Filename',
                'Random Forest Model .bin Filename Appendix',
                'Eigenvector Model .bin Filename Appendix',
                'Program Output Log .txt Filename Appendix',
                'Predictor Importance .txt Filename Appendix',
                'Minimum number of observations for viable processing',
                'Date and Time column name',
                'Tolerance around zero for delta bit and hole depths',
                'Maximum bit depth from hole bottom for drilling activity (meters)',
                'Max delta block height rate for valid rig state',
                'Max delta bit depth rate for valid rig state',
                'Max delta hole depth rate for valid rig state',
                'Max negative delta hole depth for valid rig state',
                'Target Column Name',
                'Number of Random Forest trees per Chunk',
                'Number of Random Forest chunks in run',
                'Number of trees per rf diagnostic trace',
                'Verbose log file output (Yes/No)',
                'Double training dataset to model missing EDR_RotaryTorque (Yes/No)',
                'Numeric value to substitute for missing EDR_RotaryTorque values',
                'Convert Units Of Measure (UOM) to Standard Units for random forest model',
                'Number of records used before current record for calculated predictors',
                'Number of records used after current record for calculated predictors',
                'Predictor - EDR_BitDepth to EDR_HoleDepth ratio',
                'Predictor - EDR_BitDepth speed',
                'Predictor - EDR_BitDepth distance from EDR_HoleDepth',
                'Predictor - EDR_BitDepth delta from smoothed bit depth',
                'Predictor - EDR_HoleDepth delta from smoothed hole depth',
                'Predictor - absolute EDR_BlockHeight travel distance',
                'Predictor - consecutive constant EDR_BlockHeight interval count',
                'Predictor - EDR_ToolFace runmean of abs delta value',
                'Incoming file based predictors for building calculated predictors')

  
  # Initialize parameters list
  prm.t <- list()
  # copies text parameters file verbatim
  prm.t$text <- text$text
  
  # Program Classify Train V3.1 3/21/2019
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.t$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$title <- text$v1[r]
  
  # Training Data Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$trainFilename <- text$v1[r]
  if(!(substr(prm.t$trainFilename,(nchar(prm.t$trainFilename)-3),nchar(prm.t$trainFilename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... training file must be of type ".csv"')
  }
  
  # Random Forest Model .bin Filename Appendix
  kwi <- 4 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$rfFilename <- paste(substr(prm.t$trainFilename,1,(nchar(prm.t$trainFilename)-4)),text$v1[r],sep="")
  if(!(substr(prm.t$rfFilename,(nchar(prm.t$rfFilename)-3),nchar(prm.t$rfFilename))==".bin")) {
    stop('ERROR WITH PARAMETER FILE ... random forest model output filename must be of type ".bin"')
  }
  
  # Eigenvector Model .bin Filename Appendix
  kwi <- 5 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$evFilename <- paste(substr(prm.t$trainFilename,1,(nchar(prm.t$trainFilename)-4)),text$v1[r],sep="")
  if(!(substr(prm.t$evFilename,(nchar(prm.t$evFilename)-3),nchar(prm.t$evFilename))==".bin")) {
    stop('ERROR WITH PARAMETER FILE ... eigenvector model output filename must be of type ".bin"')
  }
  
  # Program Output Log .txt Filename Appendix
  kwi <- 6 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$outputFilename <- paste(substr(prm.t$trainFilename,1,(nchar(prm.t$trainFilename)-4)),text$v1[r],sep="")
    if(!(substr(prm.t$outputFilename,(nchar(prm.t$outputFilename)-3),nchar(prm.t$outputFilename))==".txt")) {
      stop('ERROR WITH PARAMETER FILE ... program log output filename must be of type ".txt"')
    }
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.t$outputFilename <- ''
  }
  
  # Predictor Importance .txt Filename Appendix
  kwi <- 7 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  prm.t$importanceFilename <- '' # default is blank filename, which will skip writing Diagnostic file in mainClassify.R
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$importanceFilename <- paste(substr(prm.t$trainFilename,1,(nchar(prm.t$trainFilename)-4)),text$v1[r],sep="")
  }
  
  # Minimum number of observations for viable processing
  kwi <- 8 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
    prm.t$minDataRows <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Date and Time column name
  kwi <- 9 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$timeColName <- text$v1[r]

  # Tolerance around zero for delta bit and hole depths
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$deltaHoleBitZeroTolerance <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Maximum bit depth from hole bottom for drilling activity (meters)
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$maxBitDistFromHoleBottom <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }

  # Max delta block height rate for valid rig state
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$maxDeltaBlockheightRate <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Max delta bit depth rate for valid rig state
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$maxDeltaBitdepthRate <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }  
  
  # Max delta hole depth rate for valid rig state
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$maxDeltaHoledepthRate <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  } 
  
  # Max negative delta hole depth for valid rig state
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$maxNegativeDeltaHoledepth <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Target Column Name
  kwi <- 16 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$target <- text$v1[r]
  
  # Number of Random Forest trees per Chunk
  kwi <- 17 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$ntree <- as.integer(text$v1[r])
  
  # Number of Random Forest chunks in run
  kwi <- 18 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$rfchunks <- as.integer(text$v1[r])
  
  # Number of trees per rf diagnostic trace
  kwi <- 19 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$dotrace <- as.integer(text$v1[r])
  
  # Verbose log file output (Yes/No)
  kwi <- 20
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Double training dataset to model missing EDR_RotaryTorque (Yes/No)
  kwi <- 21
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.t$missingTorque <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Numeric value to substitute for missing EDR_RotaryTorque values
  kwi <- 22
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.t$missingTorqueValue <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Convert Units Of Measure (UOM) to Standard Units for random forest model
  kwi <- 23
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+2
  if (text$count[r1] !=5 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 5 bracket fields (i.e. bracket pair [])
                                        on line 2 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.t$standardUOM <- data.frame(dColName=as.character(text$v1[r1]),
                                   uomColName=as.character(text$v2[r1]),
                                   inputUnit=as.character(text$v3[r1]),
                                   outputUnit=as.character(text$v4[r1]),
                                   formula=as.character(text$v5[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==5) {
    prm.t$standardUOM <- rbind(prm.t$standardUOM,data.frame(dColName=as.character(text$v1[r1]),
                                                              uomColName=as.character(text$v2[r1]),
                                                              inputUnit=as.character(text$v3[r1]),
                                                              outputUnit=as.character(text$v4[r1]),
                                                              formula=as.character(text$v5[r1])))
    r1 <- r1+1
  }
  prm.t$standardUOM$dColName <- as.character(prm.t$standardUOM$dColName)
  prm.t$standardUOM$uomColName <- as.character(prm.t$standardUOM$uomColName)
  prm.t$standardUOM$inputUnit <- as.character(prm.t$standardUOM$inputUnit)
  prm.t$standardUOM$outputUnit <- as.character(prm.t$standardUOM$outputUnit)
  prm.t$standardUOM$formula <- as.character(prm.t$standardUOM$formula)  
 
  prm.t$standardUOM$slope <- 1
  prm.t$standardUOM$intercept <- 0
  
  # Number of records used before current record for calculated predictors
  kwi <- 24 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
    prm.t$beforeCount <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Number of records used before current record for calculated predictors
  kwi <- 25 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
    prm.t$afterCount <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Predictor - EDR_bitDepth to EDR_holeDepth ratio
  kwi <- 26 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$bitHoleDepthRatioName <- text$v1[r]
  } else { 
    prm.t$bitHoleDepthRatioName <- ''
  }
  
  # Predictor - EDR_bitDepth speed
  kwi <- 27 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$bitSpeedName <- text$v1[r]
  } else { 
    prm.t$bitSpeedName <- ''
  }
  
  # Predictor - EDR_bitDepth distance from EDR_holeDepth 
  kwi <- 28 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$bitDistFromHoleBottomName <- text$v1[r]
  } else { 
    prm.t$bitDistFromHoleBottomName <- ''
  }
  
  # Predictor - EDR_BitDepth delta from smoothed bit depth 
  kwi <- 29 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$deltaBitDepthSmooth <- text$v1[r]
  } else { 
    prm.t$deltaBitDepthSmooth <- ''
  }
  
  # Predictor - EDR_HoleDepth delta from smoothed hole depth
  kwi <- 30 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$deltaHoleDepthSmooth <- text$v1[r]
  } else { 
    prm.t$deltaHoleDepthSmooth <- ''
  }
  
  # Predictor - absolute EDR_BlockHeight travel distance
  kwi <- 31 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$deltaBlockHeightAbsName <- text$v1[r]
  } else { 
    prm.t$deltaBlockHeightAbsName <- ''
  }
  
  # Predictor - consecutive constant EDR_BlockHeight interval count
  kwi <- 32 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$blockHeightRunLengthsName <- text$v1[r]
  } else { 
    prm.t$blockHeightRunLengthsName <- ''
  }
  
  # Predictor - EDR_ToolFace runmean of abs delta value
  kwi <- 33 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.t$toolFaceAbsDeltaRunMean <- text$v1[r]
  } else { 
    prm.t$toolFaceAbsDeltaRunMean <- ''
  }
  
  # Incoming file based predictors for building calculated predictors
  kwi <- 34 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+10
  if (text$count[r1] !=8 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 7 bracket fields (i.e. bracket pair [])
                                        8 lines below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.t$cp <- data.frame(name=as.character(text$v1[r1]),
                         base=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE,
                         runmean=if (toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE,
                         runsd=if (toupper(substr(text$v4[r1],1,1))=='Y') TRUE else FALSE,
                         delta=if (toupper(substr(text$v5[r1],1,1))=='Y') TRUE else FALSE,
                         maxdeltarate=if (suppressWarnings(!(is.na(as.numeric(text$v6[r1]))))) as.numeric(text$v6[r1]) else 1e6,
                         percent=if (toupper(substr(text$v7[r1],1,1))=='Y') TRUE else FALSE,
                         eigencount=if (suppressWarnings(!(is.na(as.integer(text$v8[r1]))))) as.integer(text$v8[r1]) else 0)
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==8) {
    prm.t$cp <- rbind(prm.t$cp,data.frame(name=as.character(text$v1[r1]),
                           base=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE,
                           runmean=if (toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE,
                           runsd=if (toupper(substr(text$v4[r1],1,1))=='Y') TRUE else FALSE,
                           delta=if (toupper(substr(text$v5[r1],1,1))=='Y') TRUE else FALSE,
                           maxdeltarate=if (suppressWarnings(!(is.na(as.numeric(text$v6[r1]))))) as.numeric(text$v6[r1]) else 1e6,
                           percent=if (toupper(substr(text$v7[r1],1,1))=='Y') TRUE else FALSE,
                           eigencount=if (suppressWarnings(!(is.na(as.integer(text$v8[r1]))))) as.integer(text$v8[r1]) else 0))
    r1 <- r1+1
  }
  prm.t$cp$name <- as.character(prm.t$cp$name)
  prm.t$cp$eigencount[prm.t$cp$eigencount<0] <- 0
  prm.t$cp$eigencount[prm.t$cp$eigencount>(prm.t$beforeCount+1+prm.t$afterCount)] <- prm.t$beforeCount+1+prm.t$afterCount 
  
  # Build list of predictors to be used (input and calculated from inputs)
  prm.t$tpnames <- NULL
  if (nchar(prm.t$bitHoleDepthRatioName)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$bitHoleDepthRatioName)
  if (nchar(prm.t$bitSpeedName)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$bitSpeedName)
  if (nchar(prm.t$bitDistFromHoleBottomName)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$bitDistFromHoleBottomName)
  if (nchar(prm.t$deltaBitDepthSmooth)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$deltaBitDepthSmooth)
  if (nchar(prm.t$deltaHoleDepthSmooth)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$deltaHoleDepthSmooth)
  if (nchar(prm.t$deltaBlockHeightAbsName)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$deltaBlockHeightAbsName)
  if (nchar(prm.t$blockHeightRunLengthsName)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$blockHeightRunLengthsName)
  if (nchar(prm.t$toolFaceAbsDeltaRunMean)>0) prm.t$tpnames <- append(prm.t$tpnames,prm.t$toolFaceAbsDeltaRunMean)
  for (p in 1:nrow(prm.t$cp)) {
    if (prm.t$cp$base[p]) prm.t$tpnames <- append(prm.t$tpnames,prm.t$cp$name[p])
    if (prm.t$cp$runmean[p]) prm.t$tpnames <- append(prm.t$tpnames,paste(prm.t$cp$name[p],'_runmean',sep=''))
    if (prm.t$cp$runsd[p]) prm.t$tpnames <- append(prm.t$tpnames,paste(prm.t$cp$name[p],'_runsd',sep=''))
    if (prm.t$cp$delta[p]) prm.t$tpnames <- append(prm.t$tpnames,paste(prm.t$cp$name[p],'_delta',sep=''))
    if (prm.t$cp$percent[p]) prm.t$tpnames <- append(prm.t$tpnames,paste(prm.t$cp$name[p],'_percent',sep=''))
    if (prm.t$cp$eigencount[p]>0) {
      for (p1 in 1:prm.t$cp$eigencount[p]) {
        prm.t$tpnames <- append(prm.t$tpnames,paste(prm.t$cp$name[p],'_ev',as.character(p1),sep=''))
      }
    }
  }

  return(prm.t)
}