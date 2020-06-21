#######################################################################################
# rtpEnd.R - write files for real time predictions
# Ensign Energy Services Inc. retains all rights to this software
# FHS, June 2, 2017
#######################################################################################
rtpEnd <- function(rtp) {
  
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
    options(width=132)
  }
  
  cat('\nSys.time()=',as.character(Sys.time()))
  cat('\nTotal SQL error count=',sum(rtp$dt.output$errorCount,na.rm=T))
  cat('\nTotal SQL mismatch count=',sum(rtp$dt.output$mismatch,na.rm=T),' (if non-zero, usually because of insufficient lag delay)\n')
  
  # Write rig classification .csv file if requested (non-blank filename)
  if (nchar(rtp$prm.rp$outputResultFilename) >0 & nrow(rtp$dt.output)>0) {
    cat('\nWriting RTpredict results file with ',nrow(rtp$dt.output), 'rows and ',ncol(rtp$dt.output), 
        ' columns (csv file) : \n',rtp$prm.rp$outputResultFilename,'\n')
    write.csv(rtp$dt.output,
              file=rtp$prm.rp$outputResultFilename,row.names=FALSE)
  }
  # write rtp$dt.diag file for Diagnostics
  if (nchar(rtp$prm.rp$outputDiagFilename)>0) {
    cat('\nWriting RTpredict diagnostics file with ',nrow(rtp$dt.diag), 'rows and ',ncol(rtp$dt.diag), 
        ' columns (csv file) : \n',rtp$prm.rp$outputDiagFilename,'\n')
    write.csv(rtp$dt.diag,
              file=rtp$prm.rp$outputDiagFilename,row.names=FALSE)
  }
  if (sink.number()>0) sink(file=NULL)
  return(rtp) 
}