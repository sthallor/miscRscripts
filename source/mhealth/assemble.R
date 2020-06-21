#######################################################################################
# assemble.R routine to assemble downloaded historian file chunks
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Apr 7, 2017
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/170407_IGBT_temp_predict/rig156/'

# Assemble training dataset from clean monthly data
fname <- c('1601_clean.csv',
           '1602_clean.csv',
           '1603_clean.csv',
           '1604_clean.csv',
           '1605_clean.csv',
           '1606_clean.csv',
           '1607_clean.csv',
           '1608_clean.csv',
           '1609_clean.csv',
           '1610_clean.csv',
           '1611_clean.csv',
           '1612_clean.csv')
fname.out <- '170407_Rig156_2016All_clean.csv'
columnOrder <- c('time','igbt_temp','block_height','hookload',
                 'dc_bus_voltage','output_voltage','current','torque','speed','frequency')

# # Assemble data by month for dclean from raw historian downloads
# fname <- c('170222_DR156_IGBT_temp_Mar2016W1.csv',
#            '170222_DR156_IGBT_temp_Mar2016W2.csv',
#            '170222_DR156_IGBT_temp_Mar2016W3.csv',
#            '170222_DR156_IGBT_temp_Mar2016W4.csv',
#            '170222_DR156_IGBT_temp_Mar2016W5.csv')
# fname.out <- '170222_DR156_IGBT_temp_Mar2016.csv'

# # Assemble training dataset from clean monthly data
# fname <- c('170222_DR156_IGBT_temp_Feb2016_clean.csv',
#            '170222_DR156_IGBT_temp_Mar2016_clean.csv',
#            '170222_DR156_IGBT_temp_Apr2016_clean.csv',
#            '170222_DR156_IGBT_temp_May2016_clean.csv',
#            '170222_DR156_IGBT_temp_Jun2016_clean.csv',
#            '170222_DR156_IGBT_temp_Jul2016_clean.csv',
#            '170222_DR156_IGBT_temp_Aug2016_clean.csv',
#            '170222_DR156_IGBT_temp_Sep2016_clean.csv',
#            '170222_DR156_IGBT_temp_Oct2016_clean.csv',
#            '170222_DR156_IGBT_temp_Nov2016_clean.csv',
#            '170222_DR156_IGBT_temp_Dec2016_clean.csv')
# fname.out <- '170222_DR156_IGBT_temp_All2016_clean.csv'

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
