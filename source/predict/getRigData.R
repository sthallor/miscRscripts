#######################################################################################
# getRigData.R gets data directly from drill rig ignition database with SQL Query
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 10, 2017
#######################################################################################

start.time <- Sys.time()

library(RODBC)
ODBCConnectName <- 'rig140'

outputFilename <- 'C:/Users/Fred.Seymour/Historian_Data/170407_IGBT_temp_predict/RTrig140/1705B.csv' 
startDatetime <- '2017-05-08 15:00:00'
endDatetime <- '2017-05-10 23:59:59'

# Convert datetimes to epoch in milliseconds
startEpoch <- as.numeric(as.POSIXlt(startDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000
endEpoch <- as.numeric(as.POSIXlt(endDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000

sql_query <- paste0("select * from sql_tagdata where t_stamp between ",
                    as.character(startEpoch)," and ",as.character(endEpoch),
                    " order by t_stamp desc")

cat('\nOpening SQL connection with ',ODBCConnectName)
ch <- odbcConnect(ODBCConnectName)

cat('\nPerforming Query from=',startDatetime,' to=',endDatetime)
tags1 <- sqlQuery(ch,sql_query)

cat('\nODBC Query returned ',nrow(tags1),' records.')
cat('\nClosing RODBC Connection.')
close(ch)

if (nrow(tags1)==0) stop('Stopping, there is no data to process...')

tags1$t_stamp <- as.POSIXlt(tags1$t_stamp/1000,origin="1970-01-01 00:00:00")
tags1 <- tags1[,c('t_stamp','tagpath','floatvalue')]
colnames(tags1)[3] <- 'tagvalue'

if (!is.null(outputFilename)) {
  cat('\nWriting tag data raw Historian data file ',outputFilename)
  write.csv(tags1,file=outputFilename,row.names=FALSE)
  cat('\nwith ',nrow(tags1),' rows and ',ncol(tags1),' columns')
  cat('\nfrom date=',as.character(min(tags1$t_stamp)),' to ',as.character(max(tags1$t_stamp)))
}

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='mins')),digits=1),' minutes.\n')

