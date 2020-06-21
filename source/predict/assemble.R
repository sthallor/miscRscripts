#######################################################################################
# assemble.R routine to assemble downloaded historian file chunks
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 18, 2017
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/170518_IGBT_temp_predict/rig140_train_data/'

# Assemble training dataset from clean monthly data
fname <- c('1611_clean.csv',
           '1612_clean.csv',
           '1701_clean.csv',
           '1702_clean.csv',
           '1703_clean.csv',
           '1704_clean.csv',
           '1705_clean.csv')

fname.out <- '170518_rig140_all_clean.csv'
columnOrder <- c('time','igbt_temp','block_height','hookload',
                 'dc_bus_voltage','output_voltage','current','torque','speed','frequency')

for (f in fname) {
  dt <- read.csv(paste0(dir,f), nrow=-1)
  cat('\nRead file ',f,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')
  dt <- dt[,columnOrder]
  if (f==fname[1]) {
    dt.all <- dt
  } else {
    dt.all <- rbind(dt.all,dt)
    cat(' ... now have a total of ',nrow(dt.all),' rows.')
  }
}

# # Clean up time stamp (if needed)
# dt.all$t_stamp <- as.POSIXlt(as.character(dt.all$t_stamp),"%d-%B-%Y %H:%M:%S",tz="")

# cat('\n\nOut of ',nrow(dt.all),' rows there are ',sum(is.na(dt.all$rig.156.ensign_ac_rig.abb_dw.igbt_temp)),' NAs and ',
#     sum(dt.all$rig.156.ensign_ac_rig.abb_dw.igbt_temp>100),' igbt_temp values greater than 100C.')

write.csv(dt.all,file=paste0(dir,fname.out),row.names=FALSE)
cat('\nWrote file ',fname.out,' with ',nrow(dt.all),' rows and ',ncol(dt.all),' columns.')
