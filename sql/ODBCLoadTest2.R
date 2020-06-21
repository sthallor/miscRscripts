#######################################################################################
# Test to load data directly from historian database with SQL Query
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Apr 7, 2017
#######################################################################################

start.time <- Sys.time()

library(RODBC)
ODBCConnectName <- 'ODBC-Historian'
outputFilename <- 'C:/Users/Fred.Seymour/Historian_Data/170407_IGBT_temp_predict/rig156/1703.csv'
startDatetime <- '2017-03-01 00:00:00'
endDatetime <- '2017-03-31 23:59:59'

# Convert datetimes to epoch in milliseconds
startEpoch <- as.numeric(as.POSIXlt(startDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000
endEpoch <- as.numeric(as.POSIXlt(endDatetime,"%Y-%m-%d %H:%M:%S",tz=""))*1000

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
,'ensign_ac_rig/hmi_ai/hookload'
,'edr_data/bit_depth'
)
and drv.nice_name='156'    -- only data from rig 156
and t_stamp>=",as.character(startEpoch),
" and t_stamp<=", as.character(endEpoch))

cat('\nOpening SQL connection with ',ODBCConnectName)
ch <- odbcConnect(ODBCConnectName)

cat('\nPerforming Query from=',startDatetime,' to=',endDatetime)
tags <- sqlQuery(ch,sql_query)

cat('\nODBC Query returned ',nrow(tags),' records.')
cat('\nClosing RODBC Connection.')
close(ch)

if (nrow(tags)==0) stop('Stopping, there is no data to process...')

tags$t_stamp <- as.POSIXlt(tags$t_stamp/1000,origin="1970-01-01 00:00:00")

# cat('\nWriting tag data raw Historian data file ',outputFilename)
# write.csv(tags,file=outputFilename,row.names=FALSE)
# cat('\nwith ',nrow(tags),' rows and ',ncol(tags),' columns')
# cat('\nfrom date=',as.character(min(tags$t_stamp)),' to ',as.character(max(tags$t_stamp)))

cat('\n\nProgram Execution Elapsed time ', 
    round(as.numeric(difftime(Sys.time(),start.time,units='mins')),digits=1),' minutes.\n')