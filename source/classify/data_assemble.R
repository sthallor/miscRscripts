##########################################################################
# data_assemble.R routine to assemble individual well training files into one composite file
# Ensign Energy Services Inc. retains all rights to this software
# FHS Mar 13, 2017
##########################################################################

dir <- '/Users/Fred.Seymour/EDR_Data/170531_Train/'

holes <- c('119A',  '152A',  '155A',  '155B',  '532A',  '769A',  '785A')
dates <- c('170313','170313','170313','170313','170313','170313','170313')

trainFilename <- c('170313_train_119A.csv',
                   '170313_train_152A.csv',
                   '170313_train_155A.csv',
                   '170313_train_155B.csv',
                   '170313_train_532A.csv',
                   '170313_train_769A.csv',
                   '170313_train_785A.csv',
                   '170531_train_220A.csv')

for (i in 1:length(trainFilename)) {
  trainFilename1 <- paste0(dir,trainFilename[i])
  dt1 <- read.csv(trainFilename1)
  cat('\nread file ',trainFilename1,' with ',nrow(dt1),' rows and ',ncol(dt1),' columns')
  if (i==1) {
    dt2 <- dt1
  } else {
    if (ncol(dt1) != ncol(dt2)) {
      cat('\n\nERROR column counts do not match')
      cat('\nread file has ',ncol(dt1),' columns while build dataframe has ',ncol(dt2))
      stop()
    }
    if (sum(colnames(dt2)!=colnames(dt1))>0) { 
      cat('\n\nERROR... there is a column name mismatch!')
      cat('\nread file colnames=',colnames(dt1)[colnames(dt2)!=colnames(dt1)])
      cat('\ncorresponding dataframe colnames=',colnames(dt2)[colnames(dt2)!=colnames(dt1)])
      stop()
    }
    dt2 <- rbind(dt2,dt1)
  }
  cat (', now have ',nrow(dt2),' rows total.')
}

trainFilename2 <- paste0(dir,'170531_8_Wells_V6.csv')
write.csv(dt2,file=trainFilename2,row.names=FALSE)
cat('\nWrote file ',trainFilename2)
