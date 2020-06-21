#######################################################################################
# downtimeSearch.R - catch downtime events in EDR .csv file
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 6, 2016
#######################################################################################

# parameters are here
selectCols <- c('time','Rig','WellName','DrillingDesc','Remark')
keyword <- 'Downtime'
outFilename <- 'Downtime.csv'


# get command line arguments
arguments <- commandArgs(trailingOnly=TRUE)
if (length(arguments)==1) {
  csvFilename <- arguments[1]
} else {
  # csvFilename <- 'input_EDR_file_clean_output.csv')
  csvFilename <- '/Users/Fred.Seymour/Rcode/TestData/classify/160428_DR152_training_V3_clean_classify.csv'
  cat("\nNo command line argument ... using default parameters filename ",csvFilename,"\n\n")
}

# Read the file
dt <- read.csv(csvFilename)
cat('\nRead file ',csvFilename,'\n with ', nrow(dt),' rows and ',ncol(dt), 'columns.\n')

# Only save selected columns and rows
dt <- dt[,colnames(dt) %in% selectCols]
if (length(selectCols) > ncol(dt)) {
  # There are some missing columns that must be filled
  for (c in selectCols[!(selectCols %in% colnames(dt))]) dt[[c]] <- ''
}
dt <- dt[,selectCols]
dt <- dt[grep(keyword,dt$DrillingDesc),]

cat('\n\nFound ',nrow(dt),' downtime observations.\n\n')

if (nrow(dt) > 0) {
  # Write or append to the output file
  if (file.exists(outFilename)) {
    write.table(dt,outFilename,row.names=F,col.names=F,append=T,sep=',')
  } else {
    write.table(dt,outFilename,row.names=F,col.names=T,append=F,sep=',')
  }
}



