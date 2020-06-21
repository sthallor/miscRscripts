#######################################################################################
# get.prm.comp.R - read and decode ASCII parameters file for compare program
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 18, 2019
#######################################################################################
#
# Program compare V3.0 Program Parameters 2/18/2019

# Title  [Batch vs real time comparison 2/18/2019]
# 
# Input file1 .csv name   [C:/Users/Fred.Seymour/EDR_Data/180813_Testing/181003_Well649995/input_EDR_file_clean_output_classify1.csv]
# Input file2 .csv name   [C:/Users/Fred.Seymour/EDR_Data/180813_Testing/181003_Well649995/input_EDR_file_clean_output_classify1.csv]
# 
# Output log .txt Filename                   []
# Output numeric compare .csv name           []
# Output categories compare tables .csv name []
# Output combined selected data .csv name    []
# Output selected compareplot .pdf name      []
# 
# Time offset applied to File2 before comparison (seconds)   [0]
# Verbose logfile output (Yes/No)                            [Yes]
# 
# File1             File2
# Dataset name     [BAT5m]          [BAT0m]
# Lineplot Colors  [blue]           [red]
# 
# Plot symbol pch code (0=square, 1=circle, 2=triangle)      [1]
# Plot symbol cex size factor                                [0.5]  
# Add mismatch symbol to scatterplots & lineplots (Yes/No)   [Yes]
# Mismatch symbol pch code (3=+,4=x,8=*)                     [4]
# Mismatch symbol color                                      [black]
# Mismatch symbol size factor (cex)                          [0.5]
# Add regression lines to scatterplots (Yes/No)              [Yes]
# Regression line color                                      [green]
# Regression line type lty (2=dashed)                        [2]
# 
# Combined compare dataset index minimum (1 for first)       [1]
# Combined compare dataset index maximum (-1 for last)       [-1]
# 
# Classification Columns for Comparison
# Save - Yes/No for saving data from both file1 and file2 plus third column for matched classifications
# ColumnName    Save(Y/N)
# [rig_state]   [Yes]
# 
# Numeric Columns for Comparison
# Save - Yes/No for saving matched data from both file1 and file2
# Scatterplot - Yes/No for creating a scatter plot of file1 vs file2 columns
# Lineplot - Yes/No for creating a lineplot of file1 vs file2 columns
# Min  - Min value for scatterplot (blank for min of both file1, file2 columns
# Max  - Max value for scatterplot (blank for max of both file1, file2 columns
# Reverse - Yes/No for reversing the line chart numbers on the y-axis
# ColumnName                  Save(Y/N)   Scatterplot(Y/N) Lineplot(Y/N)  Min     Max     Reverse
# [rfvote]                    [Y]         [Y]              [Y]            [0.0]   [1.0]   [N]
# [EDR_BlockHeight]           [Y]         [Y]              [Y]            []      []      [N]
# [EDR_HookLoad]              [Y]         [Y]              [Y]            []      []      [N]
# [EDR_WOB]                   [Y]         [Y]              [Y]            []      []      [N]
# [EDR_StandpipePressure]     [Y]         [Y]              [Y]            []      []      [N]

                                                                    
get.prm.comp <- function(prmFilename) {
  # Read and decode text parameters file
  text <- read.table(prmFilename,sep="\\",
                     stringsAsFactors=FALSE,
                     blank.lines.skip=FALSE)
  text <- prm.decode(text)
  
  keywords <- c('Program compare V3.0 Program Parameters 2/18/2019',
                'Title',
                'Input file1 .csv name',
                'Input file2 .csv name',
                'Output log .txt Filename',
                'Output numeric compare .csv name',
                'Output categories compare tables .csv name',
                'Output combined selected data .csv name',
                'Output selected compareplot .pdf name',
                'Time offset applied to File2 before comparison (seconds)',
                'Verbose logfile output (Yes/No)',
                'Dataset name',
                'Lineplot Colors',
                'Plot symbol pch code',
                'Plot symbol cex size factor',
                'Add mismatch symbol to scatterplots & lineplots (Yes/No)',
                'Mismatch symbol pch code',
                'Mismatch symbol color',
                'Mismatch symbol size factor (cex)',
                'Add regression lines to scatterplots',
                'Regression line color',
                'Regression line type lty',
                'Combined compare dataset index minimum',
                'Combined compare dataset index maximum',
                'Classification Columns for Comparison',
                'Numeric Columns for Comparison')
  
  # Initialize parameters list
  prm.comp <- list()
  # copies text parameters file verbatim
  prm.comp$text <- text$text
  
  # Program join V2.0 Program Parameters 3/03/2016
  kwi <- 1 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             WRONG PROGRAM, VERSION, OR DATE... 
                             MUST EXACTLY MATCH WITH "%s"\n',keywords[kwi]))
  prm.comp$version <- keywords[kwi]
  
  # Title
  kwi <- 2 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$title <- text$v1[r]
  
  # Input file1 .csv name
  kwi <- 3 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$file1name <- text$v1[r]
  if(!(substr(prm.comp$file1name,(nchar(prm.comp$file1name)-3),nchar(prm.comp$file1name))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... must be of type .csv')
  }
  
  # Input file2 .csv name
  kwi <- 4 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$file2name <- text$v1[r]
  if(!(substr(prm.comp$file2name,(nchar(prm.comp$file2name)-3),nchar(prm.comp$file2name))==".csv")) {
    stop('ERROR WITH PARAMETER FILE ... must be of type .csv')
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
      stop('ERROR WITH PARAMETER FILE ... offset output log file name appendix must be of type .txt')
    prm.comp$outputName <- text$v1[r]
  } else { # if blank then output log filename is blank and output is sent to default device
    prm.comp$outputName <- ''
  }
  
  # Output numeric compare .csv name
  kwi <- 6 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.comp$numCompName <- text$v1[r]
    if(!(substr(prm.comp$numCompName,(nchar(prm.comp$numCompName)-3),nchar(prm.comp$numCompName))==".csv")) {
      stop('ERROR WITH PARAMETER FILE ... must be of type .csv')
    }
  } else {
    prm.comp$numCompName <- ''
  }
  
  # Output categories compare tables .csv name
  kwi <- 7 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.comp$catCompName <- text$v1[r]
    if(!(substr(prm.comp$catCompName,(nchar(prm.comp$catCompName)-3),nchar(prm.comp$catCompName))==".csv")) {
      stop('ERROR WITH PARAMETER FILE ... must be of type .csv')
    }
  } else {
    prm.comp$catCompName <- ''
  }
  
  # Output combined selected data .csv name
  kwi <- 8 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    prm.comp$combinedName <- text$v1[r]
    if(!(substr(prm.comp$combinedName,(nchar(prm.comp$combinedName)-3),nchar(prm.comp$combinedName))==".csv")) {
      stop('ERROR WITH PARAMETER FILE ... must be of type .csv')
    }
  } else {
    prm.comp$combinedName <- ''
  }
  
  # Output selected compareplot .pdf name
  kwi <- 9 # Key Word Index
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (nchar(text$v1[r])>0) {
    if(!(substr(text$v1[r],(nchar(text$v1[r])-3),nchar(text$v1[r]))==".pdf")) 
      stop('ERROR WITH PARAMETER FILE ... compareplot filename must must be of type .pdf')
    prm.comp$plotName <- text$v1[r]
  } else { # if blank then output plot filename is blank and output is sent to default device
    prm.comp$plotName <- ''
  }
  
  # Time offset applied to File2 before comparison (seconds)
  kwi <- 10
  r <- pmatch(keywords[kwi],text$text)
  prm.comp$timeOffset <- 0 # default is no offset
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(!(is.na(as.integer(text$v1[r]))))) {
      prm.comp$timeOffset <- as.integer(text$v1[r])  
    } 
  }
  
  # Verbose logfile output (Yes/No)
  kwi <- 11
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$verbose <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Dataset name
  kwi <- 12
  r <- pmatch(keywords[kwi],text$text)
  prm.comp$ds1name <- 'ds1' # default
  prm.comp$ds2name <- 'ds2' # default
  if (!is.na(r)) {
    if (text$count[r] != 2) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 2 bracket fields (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(nchar(as.character(text$v1[r]))>0)) {
      prm.comp$ds1name <- as.character(text$v1[r])  
    }
    if (suppressWarnings(nchar(as.character(text$v2[r]))>0)) {
      prm.comp$ds2name <- as.character(text$v2[r])  
    }
    if (prm.comp$ds1name==prm.comp$ds2name) stop(sprintf('ERROR WITH PARAMETER FILE ... dataset name1=%s and dataset name2=%s must be distinct',
                                                         prm.comp$ds1name, prm.comp$ds2name))
  }
  
  # Lineplot Colors
  kwi <- 13
  r <- pmatch(keywords[kwi],text$text)
  prm.comp$ds1color <- 'blue' # default
  prm.comp$ds2color <- 'red' # default
  if (!is.na(r)) {
    if (text$count[r] != 2) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 2 bracket fields (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(nchar(as.character(text$v1[r]))>0)) {
      prm.comp$ds1color <- as.character(text$v1[r])  
    }
    if (suppressWarnings(nchar(as.character(text$v2[r]))>0)) {
      prm.comp$ds2color <- as.character(text$v2[r])  
    }
    if (prm.comp$ds1color==prm.comp$ds2color) stop(sprintf('ERROR WITH PARAMETER FILE ... dataset name1=%s and dataset name2=%s must be distinct',
                                                         prm.comp$ds1color, prm.comp$ds2color))
  }  
  
  # Plot symbol pch code (0=square, 1=circle, 2=triangle)
  kwi <- 14
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$plotpchsymbol <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  if (!prm.comp$plotpchsymbol %in% 1:25) prm.comp$plotpchsymbol <- 1 # allowed symbol codess 1 to 25
  
  # Plot symbol cex size factor
  kwi <- 15
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$plotsymbolsize <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  } 
  if (prm.comp$plotsymbolsize <0.01) prm.comp$plotsymbolsize <- 0.01
  if (prm.comp$plotsymbolsize >10) prm.comp$plotsymbolsize <- 10
  
  # Add mismatch symbol to scatterplots & lineplots (Yes/No)
  kwi <- 16
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$plotmismatch <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Mismatch symbol pch code
  kwi <- 17
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$mismatchpchsymbol <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  if (!prm.comp$mismatchpchsymbol %in% 1:25) prm.comp$mismatchpchsymbol <- 1 # allowed symbol codess 1 to 25
  
  # Mismatch symbol color
  kwi <- 18
  r <- pmatch(keywords[kwi],text$text)
  prm.comp$mismatchSymbolColor <- 'blue' # default
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(nchar(as.character(text$v1[r]))>0)) {
      prm.comp$mismatchSymbolColor <- as.character(text$v1[r])  
    }
  } 
  
  # Mismatch symbol size factor (cex)
  kwi <- 19
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$mismatchSymbolSize <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  } 
  if (prm.comp$mismatchSymbolSize <0.01) prm.comp$mismatchSymbolSize <- 0.01
  if (prm.comp$mismatchSymbolSize >10) prm.comp$mismatchSymbolSize <- 10
  
  # Add regression lines to scatterplots
  kwi <- 20
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  prm.comp$plotregression <- if (toupper(substr(text$v1[r],1,1))=='Y') TRUE else FALSE
  
  # Regression line color
  kwi <- 21
  r <- pmatch(keywords[kwi],text$text)
  prm.comp$regressionLineColor <- 'blue' # default
  if (!is.na(r)) {
    if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                         text$text[r]))
    if (suppressWarnings(nchar(as.character(text$v1[r]))>0)) {
      prm.comp$regressionLineColor <- as.character(text$v1[r])  
    }
  }
  
  # Regression line type lty
  kwi <- 22
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$regressionLineType <- as.numeric(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  if (!prm.comp$regressionLineType %in% 1:6) prm.comp$mismatchpchsymbol <- 2 # lines types in range 1-6
  
  
  # Combined compare dataset index minimum
  kwi <- 23
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$indexmin <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
  
  # Combined compare dataset index maximum
  kwi <- 24
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  if (text$count[r] != 1) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 1 bracket field (i.e. bracket pair []) on line\n%s\n', 
                                       text$text[r]))
  if (suppressWarnings(!(is.na(as.numeric(text$v1[r]))))) {
    prm.comp$indexmax <- as.integer(text$v1[r])  
  } else {
    stop(sprintf('ERROR WITH PARAMETER FILE ... need numeric value in bracket field on line\n%s\n', 
                 text$text[r]))
  }
 
  # Classification Columns for Comparison
  kwi <- 25
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+3
  if (text$count[r1] !=2 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 2 bracket fields (i.e. bracket pair [])
                                        on line 3 row below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.comp$catcol <- data.frame(name=as.character(text$v1[r1]),
                                save=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE)
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==2) {
    prm.comp$catcol <- rbind(prm.comp$catcol,data.frame(name=as.character(text$v1[r1]),
                                                        save=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE))
    r1 <- r1+1
  }
  prm.comp$catcol$name <- as.character(prm.comp$catcol$name)
  
  # Numeric Columns for Comparison
  kwi <- 26
  r <- pmatch(keywords[kwi],text$text)
  if (is.na(r)) stop(sprintf('ERROR WITH PARAMETER FILE...
                             MUST HAVE EXACTLY ONE LINE STARTING WITH "%s"\n',keywords[kwi]))
  r1 <- r+8
  if (text$count[r1] !=7 ) stop(sprintf('ERROR WITH PARAMETER FILE ... need exactly 7 bracket fields (i.e. bracket pair [])
                                        on line 6 rows below keywords "%s"\n%s\n', 
                                        keywords[kwi],text$text[r1]))
  prm.comp$numcol <- data.frame(name=as.character(text$v1[r1]),
                                save=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE,
                                scatter=if (toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE,
                                line=if (toupper(substr(text$v4[r1],1,1))=='Y') TRUE else FALSE,
                                min=if (suppressWarnings(!(is.na(as.numeric(text$v5[r1]))))) as.numeric(text$v5[r1]) else -1e12,
                                max=if (suppressWarnings(!(is.na(as.numeric(text$v6[r1]))))) as.numeric(text$v6[r1]) else 1e12,
                                reverse=if (toupper(substr(text$v7[r1],1,1))=='Y') TRUE else FALSE)
  r1 <- r1+1
  while(r1 <= nrow(text) & text$count[r1]==7) {
    prm.comp$numcol <- rbind(prm.comp$numcol,data.frame(name=as.character(text$v1[r1]),
                                                        save=if (toupper(substr(text$v2[r1],1,1))=='Y') TRUE else FALSE,
                                                        scatter=if (toupper(substr(text$v3[r1],1,1))=='Y') TRUE else FALSE,
                                                        line=if (toupper(substr(text$v4[r1],1,1))=='Y') TRUE else FALSE,
                                                        min=if (suppressWarnings(!(is.na(as.numeric(text$v5[r1]))))) as.numeric(text$v5[r1]) else -1e12,
                                                        max=if (suppressWarnings(!(is.na(as.numeric(text$v6[r1]))))) as.numeric(text$v6[r1]) else 1e12,
                                                        reverse=if (toupper(substr(text$v7[r1],1,1))=='Y') TRUE else FALSE))
    r1 <- r1+1
  }
  prm.comp$numcol$name <- as.character(prm.comp$numcol$name)
  
  return(prm.comp)
}