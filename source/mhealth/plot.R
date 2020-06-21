#######################################################################################
# plot.R - Experimental plotting of igbt_tempOverheat results
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 1, 2016
#######################################################################################

# Source code directory absolute address
sourceDir <- "/home/fhs/Rcode/Ensign/"

source(paste0(sourceDir,"mhealth/mhStripchart.R"))

# from <- 1
# to <- nrow(test)
from <- 156001
to <- 157000

interval <- 1000

# Sets up parameters
prm <- list()
prm$doublebottomstripsize <- TRUE
prm$cexLabelFactor <- 4
# prm$ddname <- c('blockheight','igbt_temp','very_hot','predicted','actual_future_max_temp')
prm$ddname <- c('blockheight','hookload','rfGT100','igbt_temp')
prm$maxYlabelLength <- 23
prm$ddReverseYaxis <- c(TRUE,TRUE)
prm$title <- 'IGBT Temp RF Vote on Predictions'

for (i in seq(from=from, to=to, by=interval)) {
  mhStripchart1(test,from=i,to=(i+interval-1),prm=prm)
}

