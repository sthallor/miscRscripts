##########################################################################
# dreport.R - build rig state runs report from classifications 
# Ensign Energy Services Inc. retains all rights to this software
# FHS July 28, 2016
##########################################################################
dreport <- function(dt,prm.t,target) {
  dt[[target]] <- as.character(dt[[target]])
  dt[[target]][is.na(dt[[target]])] <- 'Data Incomplete'

  # Create rep dataframe for report
  rep <- data.frame(rig_state=rle(as.vector(dt[[target]]))$values,
                    lengths=rle(as.vector(dt[[target]]))$lengths)
  rep$from <- 0
  rep$to <- 0
  rep$to <- cumsum(rep$lengths)
  rep$from <- rep$to - rep$lengths + 1
  rep$rig <- dt$Rig[1]
  rep$startdatetime <- as.POSIXlt(dt$time[1])
  rep$enddatetime <- as.POSIXlt(dt$time[1])
  rep$elapsed_minutes <- 0
  rep$StartBitDepth <- 0
  rep$EndBitDepth <- 0
  rep$rfvote <- 0

  dt$time <- as.POSIXlt(dt$time)

  # New approach using data.table instead of indexed for loops 5/26/2016
  dt[[target]] <- as.character(dt[[target]])
  rep2 <- rle(as.vector(dt[[target]]))
  rep1 <- data.frame(values=rep2$values,lengths=rep2$lengths)
  rep2$values <- 1:length(rep2$values)
  rep1$ID <- rep2$values
  dt$ID <- inverse.rle(rep2)
  dt$time <- as.character(dt$time)
  dt$EDR_BitDepth[is.na(dt$EDR_BitDepth)] <- 0
  dt$rfvote[is.na(dt$rfvote)] <- 0
  library(data.table)
  rundata <- data.table(dt[,c('ID',target,'time','timespan','EDR_BitDepth','rfvote')])
  
  # aggregate selected results by target runs
  rep3 <- rundata[,list(startdatetime=head(time,n=1),
                        enddatetime=tail(time,n=1),
                        elapsed_minutes=round(sum(timespan/60),digits=2),
                        startBitDepth=head(EDR_BitDepth,n=1),
                        endBitDepth=tail(EDR_BitDepth,n=1),
                        rfvote=sum(rfvote)),by='ID']
  rep$startdatetime <- as.POSIXlt(rep3$startdatetime)
  rep$enddatetime <- as.POSIXlt(rep3$enddatetime)
  
  # Set enddatetime of current to match startdatetime of successor by splitting the difference
  # This takes into account the timespan for each observation which is normally 10 seconds.
  rep$startdatetime[1] <- as.POSIXlt(rep3$startdatetime[1])-5
  rep$enddatetime[nrow(rep)] <- as.POSIXlt(rep3$enddatetime[nrow(rep)])+5
  if(nrow(rep)>1) { # Make sure there is more than 1 row in rep, FHS, July 28 2016
    rep$startdatetime[2:nrow(rep)] <- as.POSIXlt((as.numeric(as.POSIXlt(rep3$startdatetime[2:nrow(rep)]))+
                                       as.numeric(as.POSIXlt(rep3$enddatetime[1:(nrow(rep)-1)])))/2,
                                       origin="1970-01-01 00:00:00") 
    rep$enddatetime[1:(nrow(rep)-1)] <- as.POSIXlt((as.numeric(as.POSIXlt(rep3$startdatetime[2:nrow(rep)]))+
                                          as.numeric(as.POSIXlt(rep3$enddatetime[1:(nrow(rep)-1)])))/2,
                                       origin="1970-01-01 00:00:00")
  }
  # elapsed minutes
  rep$elapsed_minutes <- rep3$elapsed_minutes
  # start and end bitdepths
  rep$StartBitDepth <- rep3$startBitDepth
  rep$EndBitDepth <- rep3$endBitDepth
  # rfvote
  rep$rfvote <- rep3$rfvote/rep2$lengths

  rep$rfvote[is.na(rep$rfvote)] <- 0 # Any NA rfvote values (Data Imcomplete rig state) are set to zero
 
  rep <- rep[,c('rig','startdatetime','enddatetime',
                'StartBitDepth','EndBitDepth',
                'rig_state','elapsed_minutes','rfvote','from','to')] # re-order columns
  return(rep)
}