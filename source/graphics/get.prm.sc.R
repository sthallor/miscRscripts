#######################################################################################
# get.prm.sc.R - read and decode ASCII parameters file for strip chart
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Nov 30, 2017
#######################################################################################

# Program Parameters ASCII text template
#
# Program stripchart V3.0 11/30/2017
# 
# Title  [Strip Charts]
# 
# Select drill rig filename by uncommenting (remove pound sign) on just one line
# Drill Rig .csv Input Filename [/Users/Fred.Seymour/EDR_Data/151208 Datasets/769/DR769_training_rig_states_V1.csv]
# 
# Stripchart Plot Output .pdf Filename Appendix (blank to skip) [_stripchart.pdf]
# First observation number to plot                              [1]
# Last observation number to plot (use -1 for all)              [-1]
# Number of observation for each strip chart                    [2000]
# 
# Symbol size in chart                                          [0.3]
# Label size factor (value/(number of features))                [5]
# Maximum number of characters for y-label                      [11]
# Double height of bottom strip (Yes/No)                        [No]
# Convert to UTC timezone if UTC offset available (Yes/No)      [No]
# 
# Feature/Columns for strip chart
# Reverse - reverse the numbers on the y-axis (Yes/No)
# Min - fixed minimum value for y-axis (blank for none)
# Max - fixed maximum value for y-axis (blank for none)                           
# Feature name               Reverse  Min   Max 
# [EDR_BlockHeight]          [No]     []    []
# [EDR_BitDepth]             [Yes]    []    []
# [EDR_HoleDepth]            [Yes]    []    []
# [rfvote]                   [No]     [0]   [1]
# [rfv_Other]                [No]     [0]   [1]
# [rfv_TrippingOut]          [No]     [0]   [1]
# 
# Rig State feature for color coding [rig_state]
# 
# Rig State feature color code and display parameters
# rig_state - the value of the rig state
# color     - selected color name
# pch       - selected plot symbol
# cex       - magnification factor for text
# rig_state        color     pch    cex
# [Circulating]    [black]   [16]   [1.0]
# [Connecting]     [blue]    [16]   [1.0]
# [Other]          [Red]     [16]   [1.0]
# [RDrilling]      [green]   [6]    [0.5]
# [SDrilling]      [magenta] [6]    [0.5]
# [TrippingIn]     [cyan]    [6]    [0.5]
# [TrippingOut]    [yellow2] [2]    [0.5]
# 
# Consolidate factor (non-numeric) feature categories/names/values
# feature   - factor feature/column for applying consolidation
# oldname   - old or redundant category/name/value
# newname   - new/consolidated category/name/value
# feature          oldname                       newname
# [DrillingDesc]   [Test BOP]                    [Drill]
# [DrillingDesc]   [Directional Work]            [Run casing & cementing]
# [DrillingDesc]   [Condition mud & circulate]   [REMOVE TRIP N]
# 
# Factor (non-numeric) feature category color code and display parameters
# feature   - factor feature/column for color coding
# name      - feature name
# color     - selected color name
# pch       - selected plot symbol
# cex       - magnification factor for text
# feature        name                     color      pch     cex
# [DrillingDesc] [CLEAN RIG FLO]          [red]      [16]    [1.0]
# [DrillingDesc] [Cut off Drill Line]     [red]      [16]    [1.0]
# [DrillingDesc] [Drill]                  [green]    [16]    [1.0]
# [DrillingDesc] [HELD SAFETY S]          [red]      [16]    [1.0]
# [DrillingDesc] [LINE UP FOR C]          [red]      [16]    [1.0]
# [DrillingDesc] [Nipple up BOP]          [red]      [16]    [1.0]
# [DrillingDesc] [REMOVE ROT HE]          [red]      [16]    [1.0]
# [DrillingDesc] [REMOVE TRIP N]          [black]    [16]    [1.0]
# [DrillingDesc] [Rig Service]            [red]      [16]    [1.0]
# [DrillingDesc] [Run casing & cementing] [grey]     [16]    [1.0]
# [DrillingDesc] [Trips]                  [cyan]     [16]    [1.0]
# [DrillingDesc] [Wait on cement]         [grey]     [16]    [1.0]

get.prm.sc <- function(prmFilename) {
  
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  
  text <- prm.decode(text)
  
  keywords <- c('Program stripchart V3.0 11/30/2017',
                'Title',
                'Drill Rig .csv Input Filename',
                'Stripchart Plot Output .pdf Filename Appendix',
                'First observation number to plot',
                'Last observation number to plot',
                'Number of observation for each strip chart',
                'Symbol size in chart',
                'Label size factor',
                'Maximum number of characters for y-label',
                'Double height of bottom strip',
                'Feature/Columns for strip chart',
                'Rig State feature for color coding',
                'Rig State feature color code and display parameters',
                'Consolidate factor (non-numeric) feature categories/names/values',
                'Factor (non-numeric) feature category color code and display parameters')
  
  # Initialize parameters list
  prm.sc <- list()
  # copies text parameters file verbatim
  prm.sc$text <- text$text
  
  # Program stripchart V2.0 02/26/2016
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.sc$version <- keywords[kwi]

  # Title
  kwi <- 2 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) {
    prm.sc$title <- ''
  } else {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    prm.sc$title <- text$v1[r]
  }
  
  # Drill Rig .csv Input Filename
  kwi <- 3 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi])) 
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.sc$filename <- text$v1[r]
  if(!(substr(prm.sc$filename,(nchar(prm.sc$filename)-3),nchar(prm.sc$filename))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... training parameter file must be of type .csv')
  }
  
  # Stripchart Plot Output .pdf Filename Appendix
  kwi <- 4 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                            MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                      text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.sc$plotFilename <- paste(substr(prm.sc$filename,1,(nchar(prm.sc$filename)-4)),text$v1[r],sep="")
    if(!(substr(prm.sc$plotFilename,(nchar(prm.sc$plotFilename)-3),nchar(prm.sc$plotFilename))==".pdf")) {
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
    prm.sc$from <- as.integer(text$v1[r])  
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
    prm.sc$to <- as.integer(text$v1[r])  
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
    prm.sc$interval <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need integer value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Symbol size in chart
  kwi <- 8
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.sc$cexv <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Label size factor
  kwi <- 9
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.sc$cexLabelFactor <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Maximum number of characters for y-label
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.sc$maxYlabelLength <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Double height of bottom strip
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.sc$doublebottomstripsize <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE

  # Feature/Columns for strip chart
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+5
  if (text$count[r1] !=4) stop(sprintf('ERROR WITH PARAMETER FILE ... need 4 bracket fields (i.e. bracket pair [])
                                        starting on line 5 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.sc$ddname <- text$v1[r1]
  prm.sc$ddReverseYaxis <- if(toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE
  if (suppressWarnings(!(is.na(as.numeric(text$v3[r1]))))) {
    prm.sc$ddMinYaxis <- as.numeric(text$v3[r1])  
  } else {
    prm.sc$ddMinYaxis <- -999.25 # default for blank
  }
  if (suppressWarnings(!(is.na(as.numeric(text$v4[r1]))))) {
    prm.sc$ddMaxYaxis <- as.numeric(text$v4[r1])  
  } else {
    prm.sc$ddMaxYaxis <- -999.25 # default for blank
  }

  r1 <- r1+1
  while(r1 <= nrow(text) & (text$count[r1]==4)) {
    prm.sc$ddname <- append(prm.sc$ddname,text$v1[r1])
    prm.sc$ddReverseYaxis <- append(prm.sc$ddReverseYaxis,
                                    if(toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE)
    if (suppressWarnings(!(is.na(as.numeric(text$v3[r1]))))) {
      prm.sc$ddMinYaxis <- append(prm.sc$ddMinYaxis,as.numeric(text$v3[r1]))  
    } else {
      prm.sc$ddMinYaxis <- append(prm.sc$ddMinYaxis,-999.25) # default for blank
    }
    if (suppressWarnings(!(is.na(as.numeric(text$v4[r1]))))) {
      prm.sc$ddMaxYaxis <- append(prm.sc$ddMaxYaxis,as.numeric(text$v4[r1]))  
    } else {
      prm.sc$ddMaxYaxis <- append(prm.sc$ddMaxYaxis,-999.25) # default for blank
    }
    r1 <- r1+1
  }
  
  # Rig State feature for color coding
  kwi <- 13 # keyword index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi])) 
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.sc$rig_state <- text$v1[r]

  # Rig State feature color code and display parameters
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+6
  if (text$count[r1] !=4 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 4 bracket fields (i.e. bracket pair [])
                                        on line 6 lines below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.sc$dv <- data.frame(rig_state=as.character(text$v1[r1]),
                          color=as.character(text$v2[r1]),
                          pch=as.integer(text$v3[r1]),
                          cex=as.numeric(text$v4[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==4) {
    prm.sc$dv <- rbind(prm.sc$dv,
                       data.frame(rig_state=as.character(text$v1[r1]),
                                  color=as.character(text$v2[r1]),
                                  pch=as.integer(text$v3[r1]),
                                  cex=as.numeric(text$v4[r1])))
    r1 <- r1+1
  }
  
  # Consolidate factor (non-numeric) feature categories/names/values
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+5
  if (text$count[r1] !=3 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 3 bracket fields (i.e. bracket pair [])
                                        on line five lines below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.sc$cs <- data.frame(ddname=text$v1[r1],old=text$v2[r1],new=text$v3[r1])
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==3) {
    prm.sc$cs <- rbind(prm.sc$cs,
                       data.frame(ddname=text$v1[r1],old=text$v2[r1],new=text$v3[r1]))
    r1 <- r1+1
  }
  
  # Factor (non-numeric) feature category color code and display parameters
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+7
  if (text$count[r1] !=5 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 5 bracket fields (i.e. bracket pair [])
                                        on line 7 lines below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.sc$dfv <- data.frame(ddname=as.character(text$v1[r1]),
                          factor=as.character(text$v2[r1]),
                          color=as.character(text$v3[r1]),
                          pch=as.integer(text$v4[r1]),
                          cex=as.numeric(text$v5[r1]))
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==5) {
    prm.sc$dfv <- rbind(prm.sc$dfv,
                       data.frame(ddname=as.character(text$v1[r1]),
                                  factor=as.character(text$v2[r1]),
                                  color=as.character(text$v3[r1]),
                                  pch=as.integer(text$v4[r1]),
                                  cex=as.numeric(text$v5[r1])))
    r1 <- r1+1
  }
  
  return(prm.sc)
}