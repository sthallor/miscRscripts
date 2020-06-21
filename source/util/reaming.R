#######################################################################################
# reaming.R script to add 'BackReaming' and 'Reaming' rig classification states
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 20, 2016
#######################################################################################

start.time <- Sys.time()

inFile <- '/Users/Fred.Seymour/EDR_Data/160520_Train/160520_DR785_training_V3_clean.csv'
outFile <- '/Users/Fred.Seymour/EDR_Data/160520_Train/160520_DR785_training_V4_clean.csv'

dt <- read.csv(inFile,nrows=-1)

# Calculate some fields
dt$OffBottom <- dt$EDR_HoleDepth - dt$EDR_BitDepth
dt$DeltaBlockHeight <- 0
dt$DeltaBlockHeight[2:nrow(dt)] <- dt$EDR_BlockHeight[2:nrow(dt)] - dt$EDR_BlockHeight[1:(nrow(dt)-1)]

print(table(dt$rig_state))

# Display some histograms
hist(dt$EDR_RotaryRPM[dt$rig_state=='Circulating'],breaks=100)
hist(dt$EDR_StandpipePressure[dt$rig_state=='Circulating'],breaks=100)
hist(dt$OffBottom[dt$rig_state=='Circulating'],breaks=100)
hist(dt$DeltaBlockHeight[dt$rig_state=='Circulating'],breaks=100)

# Selections for conversion
selectBackReaming <- dt$rig_state=='Circulating' &
                     dt$EDR_RotaryRPM>1 &
                     dt$EDR_StandpipePressure>100 &
                     dt$OffBottom>2 &
                     dt$DeltaBlockHeight>0
cat('\nRig state count to be reclassified as "BackReaming" = ',sum(selectBackReaming))

selectReaming <- dt$rig_state=='Circulating' &
  dt$EDR_RotaryRPM>1 &
  dt$EDR_StandpipePressure>100 &
  dt$OffBottom>1 &
  dt$DeltaBlockHeight<0 
cat('\nRig state count to be reclassified as "Reaming" = ',sum(selectReaming))

# Convert rig states and clean up
dt$rig_state <- as.character(dt$rig_state)
dt$rig_state[selectBackReaming] <- 'BackReaming'
dt$rig_state[selectReaming] <- 'Reaming'
print(table(dt$rig_state))
dt$OffBottom <- NULL
dt$DeltaBlockHeight <- NULL

# write.csv(dt,file=outFile,row.names=FALSE)
