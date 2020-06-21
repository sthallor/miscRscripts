#######################################################################################
# plot.R - Experimental plotting of igbt_tempOverheat results
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 20, 2016
#######################################################################################

# Source code directory absolute address
sourceDir <- "C:/Users/Fred.Seymour/Rcode/master/"

source(paste0(sourceDir,"mhealth/mhStripchart.R"))
source(paste0(sourceDir,"mhealth/mhStripchart1.R"))

from <- 48001
to <-   65000
# to <- nrow(test)
interval <- 1000

# Sets up parameters
prm <- list()
prm$doublebottomstripsize <- TRUE
prm$cexLabelFactor <- 4
prm$ddname <- c('blockheight','igbt_temp','very_hot','predicted','actual_future_max_temp')
prm$maxYlabelLength <- 23
prm$ddReverseYaxis <- c(TRUE,TRUE)
prm$title <- 'IGBT Temp RF Vote on Predictions'



# mhStripchart(test,from=from,to=to,prm=prm)

# Setup actual igbt temperature categories
test$igbt <- 'cool'
test$igbt[test$igbt_temp>50] <- 'warm'
test$igbt[test$igbt_temp>80] <- 'hot'
test$igbt[test$igbt_temp>100] <- 'very_hot'



for (i in seq(from=from, to=to, by=interval)) {
  mhStripchart1(test,from=i,to=(i+interval-1),prm=prm)
}

