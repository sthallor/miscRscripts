#######################################################################################
# dataedit.R script to modify a file 
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Dec 14, 2016
#######################################################################################

# Load file
dir <- '/Users/Fred.Seymour/EDR_Data/161130_Train/'
inputFilename <- paste0(dir,'161201_7_Wells_V5.csv')
dt <- read.csv(inputFilename)
cat('\nLoaded ',inputFilename,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')

# Display some initial statistics
cat('\n\nThere are a total of ',sum(dt$rig_state=='Connecting'),' Connecting rig_state rows')
cat('\n',sum(dt$EDR_PumpSPMTotal==0 & dt$rig_state=='Connecting'),
    ' Connecting rows with EDR_PumpSPMTotal==0')
cat('\n',sum(dt$EDR_PumpSPMTotal==1 & dt$rig_state=='Connecting'),
    ' Connecting rows with EDR_PumpSPMTotal==1')
cat('\n',sum(dt$EDR_PumpSPMTotal==2 & dt$rig_state=='Connecting'),
    ' Connecting rows with EDR_PumpSPMTotal==2')
cat('\n',sum(dt$EDR_PumpSPMTotal>2 & dt$rig_state=='Connecting'),
    ' Connecting rows with EDR_PumpSPMTotal>2')

# Setup run lengths of identical EDR_PumpSPMTotal values as rlePump
rlePump <- data.frame(lengths=rle(dt$EDR_PumpSPMTotal)$lengths)
rlePump$values <- rlePump$lengths
dt$rlePump <- inverse.rle(rlePump)

# Display data with run length constraint
# with 10s intervals, <4 is 30 second max, <7 is 1 minute max, <61 is 10 minute max)
cat('\n\n',sum(dt$EDR_PumpSPMTotal==1 & dt$rig_state=='Connecting' & dt$rlePump<4),
    ' Connecting rows with EDR_PumpSPMTotal==1 & rlePump<4')
cat('\n',sum(dt$EDR_PumpSPMTotal==1 & dt$rig_state=='Connecting' & dt$rlePump<7),
    ' Connecting rows with EDR_PumpSPMTotal==1 & rlePump<7')
cat('\n',sum(dt$EDR_PumpSPMTotal==1 & dt$rig_state=='Connecting' & dt$rlePump<61),
    ' Connecting rows with EDR_PumpSPMTotal==1 & rlePump<61')

# Setup selection constraint
selected <- (dt$rig_state=='Connecting' & dt$EDR_PumpSPMTotal>1) |
            (dt$rig_state=='Connecting' & dt$EDR_PumpSPMTotal==1 & dt$rlePump<4)

# # Flag for stripchart inspection
# dt$flag <- 0
# dt$flag[selected] <- 1
# 
# # Not selected based on run length for stripchart inspection
# notSelected <- dt$rig_state=='Connecting' & dt$EDR_PumpSPMTotal==1 & dt$rlePump>3
# dt$flag[notSelected] <- 2

# Actual converstion of rig states
dt$rig_state[selected] <- 'Circulating'
cat('\nNumber of Connecting records changed to Circulating is ', sum(selected))

dt$rlePump <- NULL  # Remove extra column

# # Save new file
# outputFilename <- paste0(dir,'161215_7_Wells_V5.csv')
# write.csv(dt,file=outputFilename,row.names=FALSE)
# cat('\n\nSaved ',outputFilename,' with ',nrow(dt),' rows and ',ncol(dt),' columns.')
# cat('\n\nDone.')

