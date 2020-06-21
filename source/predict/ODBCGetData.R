#######################################################################################
# Get data directly from historian database with SQL Query
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 18, 2017
#######################################################################################

start.time <- Sys.time()

library(RODBC)
ODBCConnectName <- 'ODBC-Historian'
outputFilename <- 'C:/Users/Fred.Seymour/Historian_Data/170518_IGBT_temp_predict/rig140_train_data/1705.csv'

startDatetime <- '2017-05-01 00:00:00'
endDatetime <- '2017-05-31 23:59:59'

# Convert datetimes to epoch in milliseconds
startEpoch <- as.numeric(as.POSIXlt(startDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000
endEpoch <- as.numeric(as.POSIXlt(endDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000


# Changed hookload tag to 'ensign_ac_rig/dw/rig_hookload' FHS, May 15, 2017
sql_query <- paste0("select d.t_stamp,te.tagpath,isnull(d.floatvalue,d.intvalue) as tagvalue
from IgnitionEnterpriseReporting.dbo.sqlth_1_data d
inner join IgnitionEnterpriseReporting.dbo.sqlth_te te on d.tagid=te.id
inner join IgnitionEnterpriseReporting.dbo.sqlth_scinfo sc on te.scid=sc.id
inner join IgnitionEnterpriseReporting.dbo.sqlth_drv drv on sc.drvid=drv.id
where te.tagpath in ('ensign_ac_rig/abb_dw/igbt_temp'
,'ensign_ac_rig/abb_dw/dc_bus_voltage'
,'ensign_ac_rig/abb_dw/output_voltage'
,'ensign_ac_rig/abb_dw/current'
,'ensign_ac_rig/abb_dw/torque'
,'ensign_ac_rig/abb_dw/speed'
,'ensign_ac_rig/abb_dw/frequency'
,'ensign_ac_rig/dw/block_height'
,'ensign_ac_rig/dw/rig_hookload'
,'edr_data/bit_depth'
)
and drv.nice_name='140'    -- only data from rig 140
and t_stamp>=",as.character(startEpoch),
" and t_stamp<=", as.character(endEpoch))

cat('\nOpening SQL connection with ',ODBCConnectName)
ch <- odbcConnect(ODBCConnectName)

cat('\nPerforming Query from=',startDatetime,' to=',endDatetime)
tags <- sqlQuery(ch,sql_query)

cat('\nODBC Query returned ',nrow(tags),' records.')
cat('\nClosing RODBC Connection.')
close(ch)

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='secs')),digits=1),' seconds.\n')

if (nrow(tags)==0) stop('Stopping, there is no data to process...')

tags$t_stamp <- as.POSIXlt(tags$t_stamp/1000,origin="1970-01-01 00:00:00")

cat('\nWriting tag data raw Historian data file ',outputFilename)
write.csv(tags,file=outputFilename,row.names=FALSE)
cat('\nwith ',nrow(tags),' rows and ',ncol(tags),' columns')
cat('\nfrom date=',as.character(min(tags$t_stamp)),' to ',as.character(max(tags$t_stamp)))

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='mins')),digits=1),' minutes.\n')
