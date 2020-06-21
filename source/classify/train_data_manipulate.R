##########################################################################
# train_data_manipulate.R standalone program for manipulating training data
# Ensign Energy Services Inc. retains all rights to this software
# FHS Oct 17, 2018
##########################################################################

##########################################################################
# Load training file and split it into one file per Rig
# for testing with the classify program
# the rig_state field is renamed as rig_state_train
# so that it can be compared with the classify rigstate field

filetrain <- 'C:/Users/Fred.Seymour/EDR_Data/180929_Train/181016_10_wells_V8.csv'
dt <- read.csv(filetrain)
dt$time <- as.POSIXlt(dt$time,"%Y-%m-%d %H:%M:%S",tz="")
dt$Rig <- as.character(dt$Rig)
colnames(dt)[colnames(dt)=='rig_state'] <- 'rig_state_train'
cat('\nRead training file ',filetrain,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')


rignames <- as.character(unique(dt$Rig))
for (rig in rignames) {
  dtsub <- dt[dt$Rig==rig,]
  filetrainsub <- paste(substr(filetrain,1,(nchar(filetrain)-4)),"_",rig,".csv",sep="")
  cat('\nTraining data from rig ',rig,' has ',nrow(dtsub),' observations')
  write.csv(dtsub,file=filetrainsub,row.names=FALSE)
  cat('\nSaved as file: ',filetrainsub)
}



# filetrain2 <- 'C:/Users/Fred.Seymour/EDR_Data/170531_Train/170816_8_wells_V7.csv'
# dttrain2 <- read.csv(filetrain2)
# dttrain2$time <- as.POSIXlt(dttrain2$time,"%Y-%m-%d %H:%M:%S",tz="")
# 
# print(table(dttrain1$rig_state,dttrain2$rig_state))

# filepdf <- 'C:/Users/Fred.Seymour/EDR_Data/170531_Train/170531_8_wells_V6.pdf'
# if (!(filepdf=='')) pdf(file=filepdf)
# 
# for (rig in as.character(unique(dttrain$Rig))) {
#   
#   cat('\nPlotting rig=',rig)
# 
#   plot(dttrain$time[dttrain$Rig==rig & dttrain$rig_state!='Connecting'],
#        dttrain$EDR_HookLoad[dttrain$Rig==rig & dttrain$rig_state!='Connecting'],
#        pch='.',col='red',ylim=c(0,350), ylab='HookLoad',xlab='Time',
#        main=sprintf('Rig=%s UOM=%s',rig,as.character(unique(dttrain$Hookload_UOM[dttrain$Rig==rig]))))
#   
#   points(dttrain$time[dttrain$Rig==rig & dttrain$rig_state=='Connecting'],
#          dttrain$EDR_HookLoad[dttrain$Rig==rig & dttrain$rig_state=='Connecting'],
#          pch=20,col='blue')
#   
#   legend('topleft',c('Connecting','non-Connecting'),fill=c('blue','red'))
# 
# }
# 
# if (!(filepdf=='')) dev.off()

