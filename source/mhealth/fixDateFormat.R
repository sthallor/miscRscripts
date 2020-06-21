#######################################################################################
# fixDateFormat.R - reformats incoming historian date from "dd-mmm-yyyy" to "yyyy-mm-dd"
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 12, 2016
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filenameIn <- '160503_DR156_abb_dw.csv'
filenameOut <- '160503_DR156_abb_dw_A.csv'

dt <- read.csv(paste0(dir,filenameIn), nrow=-1)

dt$t_stamp <- as.character(as.POSIXlt(dt$t_stamp,"%d-%b-%Y %H:%M:%S",tz=""))

write.csv(dt,file=paste0(dir,filenameOut),row.names=FALSE)