##########################################################################
# postprocess2.R - post random forest rig state classification changes
# Ensign Energy Services Inc. retains all rights to this software
# FHS Oct 20, 2018
##########################################################################
postprocess2 <- function(dt,prm.t,prm.rc) {
  if (prm.rc$ppflag==FALSE) return(dt)
  if (is.null(dt[[prm.rc$pp_changeColname]])) {
    cat('\n\npostprocess2 WARNING .. rig state change column ',prm.rc$pp_changeColname,' not found - Ignoring.\n')
    return(dt)
  }
  
  # Creates new column target_pp and fills it with initial rf target values
  # creates run list of target & pp_changeColname
  # loops through run list and modifies target_pp according to pp_changeRigState list
  
  dt[[prm.rc$target_pp]] <- as.character(dt[[prm.t$target]])
  
  ra <- data.frame(values=rle(as.vector(paste0(as.character(dt[[prm.t$target]]),as.character(dt[[prm.rc$pp_changeColname]]))))$values,
                   lengths=rle(as.vector(paste0(as.character(dt[[prm.t$target]]),as.character(dt[[prm.rc$pp_changeColname]]))))$lengths)
  ra$values <- as.character(ra$values)
  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1

  for (i in 1:nrow(ra)) {
    j <- which(prm.rc$pp_changeRigState$oldRigState %in% as.character(dt[[prm.t$target]][ra$from[i]]) &
                 prm.rc$pp_changeRigState$changeCode %in% as.character(dt[[prm.rc$pp_changeColname]][ra$from[i]]))
    if (length(j)>0) {
      j <- j[1]
      dt[[prm.rc$target_pp]][ra$from[i]:ra$to[i]] <- prm.rc$pp_changeRigState$newRigState[j]
    }
  }
  
  if (sum(dt[[prm.rc$target_pp]]==dt[[prm.t$target]])==nrow(dt)) {
    cat('\n\nThere were no post processing rig state replacements\n')
  } else {
    cat('\n\nPost processing rig state replacement counts')
    cat('\nReplacement plus original rigstate ',prm.rc$target_pp, ' in table rows below')
    cat('\nOriginal rig states: ',prm.t$target, ' in table columns below')
    print(table(dt[[prm.rc$target_pp]],dt[[prm.t$target]]))
  }
  
  return(dt)
}