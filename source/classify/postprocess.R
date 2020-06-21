##########################################################################
# postprocess.R - post random forest consolidation of rig state classifications
# Ensign Energy Services Inc. retains all rights to this software
# FHS Oct 20, 2018
##########################################################################
postprocess <- function(dt,rep,prm.rc,prm.t,rf.votes) {
  if (prm.rc$ppflag==FALSE) return(dt)
  
  prm.rc$pptest$Count <- 0
  prm.rc$pptest$OldRFVote <- 0
  prm.rc$pptest$NewRFVote <- 0
  prm.rc$pptest$Count1 <- 0

  rep$modified <- FALSE
  for (i in 1:nrow(rep)) {
    if (!is.na(rep$rig_state[i])) {
      for (j in 1:nrow(prm.rc$pptest)) {
        # Check if current state matches and is below elapsed minutes
        if(rep$rig_state[i] == prm.rc$pptest$csn[j] &
           rep$elapsed_minutes[i] < prm.rc$pptest$csem[j]) {
          # check if before/after states match within time distance
          if (ftd(rep,prm.rc$pptest$bsn[j],prm.rc$pptest$bsdm[j],i,'b') &
              ftd(rep,prm.rc$pptest$asn[j],prm.rc$pptest$asdm[j],i,'a')) {
            rep$rig_state[i] <- prm.rc$pptest$csnn[j]
            prm.rc$pptest$Count[j] <- prm.rc$pptest$Count[j]+1
            prm.rc$pptest$OldRFVote[j] <- prm.rc$pptest$OldRFVote[j]+
              sum(rf.votes[[prm.rc$pptest$csn[j]]][rep$from[i]:rep$to[i]])
            prm.rc$pptest$NewRFVote[j] <- prm.rc$pptest$NewRFVote[j]+
              sum(rf.votes[[prm.rc$pptest$csnn[j]]][rep$from[i]:rep$to[i]])
            prm.rc$pptest$Count1[j] <- prm.rc$pptest$Count1[j]+rep$to[i]-rep$from[i]+1
            rep$modified[i] <- TRUE
            break # Only allow one change for ith drilling state   					   	
          } # if statement of before/after drilling states				   	
        } # if statement for current drilling state
      } # j loop for pptests
    } # if statement for NA value
  } # i loop for current states
  
  # Put post processing cleanup back into dt
  for (i in 1:nrow(rep)) {
    dt[[prm.t$target]][rep$from[i]:rep$to[i]] <- rep$rig_state[i]
  }
  # modify the random forest vote counts according to new picks
  dt$rfvote <- 0
  for (c in colnames(rf.votes)) {
    dt$rfvote[dt[[prm.t$target]]==c] <- rf.votes[[c]][dt[[prm.t$target]]==c]
  }

  rep$count <- rep$to - rep$from + 1
  if(prm.rc$verbose) {
    cat("\n\nPost-Processing modified ",sum(rep$modified),
        " classifications (",round(100*sum(rep$modified)/nrow(rep),digits=1),
        "%) out of ",nrow(rep)," total.\n") 
    cat("Post-Processing modified ",sum(rep$count[rep$modified==TRUE]),
        " observations (",round(100*sum(rep$count[rep$modified==TRUE])/nrow(dt),digits=1),
        "%)out of ",nrow(dt)," total. \n")
    cat("Post-Processing modified ",sum(rep$elapsed_minutes[rep$modified==TRUE]),
        " minutes (",round(100*sum(rep$elapsed_minutes[rep$modified==TRUE])/sum(rep$elapsed_minutes),digits=1),
        "%) out of ",sum(rep$elapsed_minutes)," total.\n\n")
  }
  
  # Prepare RF vote averages for display
  for (j in 1:nrow(prm.rc$pptest)) {
    if (prm.rc$pptest$Count1[j] > 0) {
      prm.rc$pptest$OldRFVote[j] <- round(prm.rc$pptest$OldRFVote[j]/prm.rc$pptest$Count1[j],digits=3)
      prm.rc$pptest$NewRFVote[j] <- round(prm.rc$pptest$NewRFVote[j]/prm.rc$pptest$Count1[j],digits=3)
    }
  }
  prm.rc$pptest$Count1 <- NULL
  
  if (prm.rc$verbose) print(prm.rc$pptest)
  
  # Reject rig state classifications when there is bad data
  # delta BlockHeight, BitDepth, or Holedepth exceeds a threshold

  if (!is.null(dt$timespan)) {
    dt[[prm.t$target]] <- as.character(dt[[prm.t$target]])
    if(!is.null(dt$EDR_BlockHeight)) {
      temp <- rep(0,nrow(dt))
      temp[2:nrow(dt)] <- abs(dt$EDR_BlockHeight[2:nrow(dt)]-dt$EDR_BlockHeight[1:(nrow(dt)-1)])
      rejects <- which(temp/dt$timespan>prm.t$maxDeltaBlockheightRate)
      if (length(rejects)>0) {
        if (prm.rc$verbose) cat('\nWarning postprocess setting ',length(rejects),
            ' rig states to "Error_In_Data" due to delta block height rate exceeding ',
            prm.t$maxDeltaBlockheightRate,' m/s threshold')
        dt[[prm.t$target]][rejects] <- 'Error_In_Data'
      }
    }
    if(!is.null(dt$EDR_BitDepth)) {
      temp <- rep(0,nrow(dt))
      temp[2:nrow(dt)] <- abs(dt$EDR_BitDepth[2:nrow(dt)]-dt$EDR_BitDepth[1:(nrow(dt)-1)])
      rejects <- which(temp/dt$timespan>prm.t$maxDeltaBitdepthRate)
      if (length(rejects)>0) {
        if (prm.rc$verbose) cat('\nWarning postprocess setting ',length(rejects),
            ' rig states to "Error_In_Data" due to delta bit depth rate exceeding ',
            prm.t$maxDeltaBitdepthRate,' m/s threshold')
        dt[[prm.t$target]][rejects] <- 'Error_In_Data'
      }
    }
    if(!is.null(dt$EDR_HoleDepth)) {
      temp <- rep(0,nrow(dt))
      temp[2:nrow(dt)] <- dt$EDR_HoleDepth[2:nrow(dt)]-dt$EDR_HoleDepth[1:(nrow(dt)-1)]
      rejects <- which(temp/dt$timespan>prm.t$maxDeltaHoledepthRate)
      if (length(rejects)>0) {
        if (prm.rc$verbose) cat('\nWarning postprocess setting ',length(rejects),
            ' rig states to "Error_In_Data" due to delta hole depth rate exceeding ',
            prm.t$maxDeltaHoledepthRate,' m/s threshold')
        dt[[prm.t$target]][rejects] <- 'Error_In_Data'
      }
      rejects <- which(temp<(-abs(prm.t$maxNegativeDeltaHoledepth)))
      if (length(rejects)>0) {
        if (prm.rc$verbose) cat('\nWarning postprocess setting ',length(rejects),
            ' rig states to "Error_In_Data" due to negative delta hole depth exceeding ',
            (-abs(prm.t$maxNegativeDeltaHoledepth)),' m threshold')
        dt[[prm.t$target]][rejects] <- 'Error_In_Data'
      }
    }
    # test for NA rig states, October 20, 2018
    rejects <- which(is.na(dt[[prm.t$target]]))
    if (length(rejects)>0) {
      if (prm.rc$verbose) cat('\nWarning postprocess setting ',length(rejects),
                              ' rig states to "Error_In_Data" that are NA')
      dt[[prm.t$target]][rejects] <- 'Error_In_Data'
    }
    
    dt[[prm.t$target]] <- as.factor(dt[[prm.t$target]])
  }
  
  return(dt)
}