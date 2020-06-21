# dev.off()

rm(list=ls())

library(RODBC)

# ch <- odbcConnect("IgnitionEnterpriseReporting")
# ODBCConnectName <- 'ODBC-Historian'
ch <- odbcConnect("ODBC-Historian")



edr_query<-paste0("declare @ms_in_day bigint = 60 * 60 * 24 * 1000 /* constant - milliseconds in a day */
                  
                  select datetimeUTCbigInt,edr_dt_utc,blockheight from (
                  
                  select convert(bigint,(@ms_in_day * datediff(day,'1970-01-01',x.edr_dt_utc) - datediff(millisecond,x.edr_dt_utc, cast(edr_dt_utc as date)))) as datetimeUTCbigInt,edr_dt_utc,blockheight
                  
                  from (select dateadd(hour,5,convert(datetime,MeasurementDateTime)) as edr_dt_utc,BlockHeight from ignrpt_blackgold.blackgold_prod.dbo.welledr where well_wellid=427401) x
                  
                  ) y 
                  
                  where y.datetimeUTCbigInt between 1504524800000 and 1504569750000
                  
                  order by 1")

edr_data<-sqlQuery(ch,edr_query)



historian_query<-paste0("select t_stamp as t_stamp,floatvalue from IgnitionEnterpriseReporting.dbo.sqlth_1_data where tagid=142856 and t_stamp between 1504524800000 and 1504569750000 and dataintegrity=192")

historian_data<-sqlQuery(ch,historian_query)



plot(edr_data$datetimeUTCbigInt,edr_data$blockheight,type="l",col="grey",main="BH: EDR is grey, Historian is red")

lines(historian_data$t_stamp,historian_data$floatvalue,col="red")

segments(1504527490000,107,1504527490000,110,col="blue")

segments(1504527215010,107,1504527215010,110,col="black")

segments(1504535080000,107,1504535080000,110,col="blue")

segments(1504534796246,107,1504534796246,110,col="black")



odbcClose(ch)
