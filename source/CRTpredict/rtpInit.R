#######################################################################################
# rtpInit.R - initialization routine for real time predictions
# designed to be callable from C
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 10, 2017
#######################################################################################


rtpInit <- function(rtp) {
  # Load required libraries and functions
  source(paste0(rtp$sourceDir,"source/CRTpredict/prm.decode.R"))
  source(paste0(rtp$sourceDir,"source/CRTpredict/get.prm.CRTpredict.R"))
  source(paste0(rtp$sourceDir,"source/CRTpredict/get.prm.predictTrain.R"))
  source(paste0(rtp$sourceDir,"source/CRTpredict/get.prm.dc.R")) # for dclean
  
  start.time <- Sys.time()
  
  # Get the real time predic parameters
  cat('\nLoading real time prediction parameters from file ',rtp$prmFilename,'\n')
  rtp$prm.rp <- get.prm.RTpredict(rtp$prmFilename)

  cat('\nLoading previous training parameters from file ',rtp$prm.rp$trainPrmFilename,'\n')
  rtp$prm.pt <- get.prm.predictTrain(rtp$prm.rp$trainPrmFilename)
  
  if (nchar(rtp$prm.rp$dcleanPrmFilename)>0) {
    cat('\nLoading data clean parameters from file ',rtp$prm.rp$dcleanPrmFilename,'\n')
    rtp$prm.dc <- get.prm.dc(rtp$prm.rp$dcleanPrmFilename)
  }
  
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    cat('\nMaking predictions and sending output to:\n',rtp$prm.rp$outputLogFilename,'\n')
    sink(file=rtp$prm.rp$outputLogFilename,append=FALSE)
    options(width=132)
  }
  
  cat(rtp$prm.rp$version,'\nRun Started at : ',as.character(start.time),'\n')
  
  cat('\nVerbatim listing of real time predict parameters file from:\n',rtp$prmFilename,'\n\n')
  cat(paste(rep('-',80),collapse=''),'\n')
  for (i in 1:length(rtp$prm.rp$text)) { cat(rtp$prm.rp$text[i],'\n') }
  cat(paste(rep('-',80),collapse=''),'\n')
  
  cat('\nVerbatim listing of predict training parameters file from:\n',rtp$prm.rp$trainPrmFilename,'\n\n')
  cat(paste(rep('-',80),collapse=''),'\n')
  for (i in 1:length(rtp$prm.pt$text)) { cat(rtp$prm.pt$text[i],'\n') }
  cat(paste(rep('-',80),collapse=''),'\n')
  
  if (nchar(rtp$prm.rp$dcleanPrmFilename)>0) {
    cat('\nVerbatim listing of dclean parameters file from:\n',rtp$prm.rp$dcleanPrmFilename,'\n\n')
    cat(paste(rep('-',80),collapse=''),'\n')
    for (i in 1:length(rtp$prm.dc$text)) { cat(rtp$prm.dc$text[i],'\n') }
    cat(paste(rep('-',80),collapse=''),'\n')
  }
  
  cat('\nLoading the Random Forest Classification Model (binary file) : \n',rtp$prm.pt$rfFilename)
  load(file=rtp$prm.pt$rfFilename)
  rtp$rf <- rf
  rf <- NULL
  cat('\nThe memory consumed by the model is ');print(object.size(rtp$rf),units='Mb')
  cat('\nThe model had a total of ',length(rtp$rf$y),' training observations.')
  cat('\n',length(which(rtp$rf$y>=rtp$prm.pt$targetAlarmCutoff)),
      ' observations above ',rtp$prm.pt$targetSource,
      ' cutoff =',rtp$prm.pt$targetAlarmCutoff,' and ',
      length(which(rtp$rf$y<rtp$prm.pt$targetAlarmCutoff)),' below cutoff')
  cat('\nntree=',rtp$prm.pt$ntree,' mtry=',rtp$prm.pt$mtry)
  
  
  rtp$dt.output <- data.frame(time=NULL,             # 10 sec interval timestamp for alarm prediction record  
                              epoch=NULL,            # 10 sec interval timestamp in epoch format (milliseconds)
                              obsCount=NULL,         # count of available previous to current observations
                              igb_tempRunMean=NULL,  # running mean of igbt_temp over previous observations
                              rfTempPredict=NULL,    # random forest predicted maximum igbt_temp
                              rfGTcut=NULL,          # proportion of random forest tree prediction above alarm cutoff
                              alarm=NULL,            # alarm flag
                              ntry=NULL,             # number of SQL attempts prior to current record
                              errorCount=NULL,       # number of SQL errors prior to current record
                              mismatch=NULL,         # number of mismatched values between current SQL buffer and previous
                              processTime=NULL,      # processing time for current record including SQL query
                              lagTime=NULL)          # difference between Sys.time() and time in seconds
  rtp$dt.diag <- data.frame()
  rtp$dt.all <- NULL

  cat('\nProcessing for real time predictions begins here...\n')
  
  if (sink.number()>0) sink(file=NULL)
  return(rtp)
}