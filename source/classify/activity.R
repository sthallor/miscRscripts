##############################################################################
# activity.R - build dt$activity column from bitdepth, holdepth, and rig state
# Build data model for both training and classification
# Ensign Energy Services Inc. retains all rights to this software
# FHS March 19, 2019
##############################################################################
activity <- function(dt,prm.t,prm.rc) {
  
  # Setup the initial activity column based on holedepth and bitdepth information
  dt$activity <- 'NotDrilling'
  dt$activity[dt[[prm.t$bitDistFromHoleBottomName]]>=prm.t$maxBitDistFromHoleBottom & dt$deltaBitDepthSmooth>prm.t$deltaHoleBitZeroTolerance] <- 'TrippingIn'
  dt$activity[dt[[prm.t$bitDistFromHoleBottomName]]>=prm.t$maxBitDistFromHoleBottom & dt$deltaBitDepthSmooth<(-prm.t$deltaHoleBitZeroTolerance)] <- 'TrippingOut'
  dt$activity[dt$EDR_BitDepth_delta>prm.t$deltaHoleBitZeroTolerance] <- 'Drilling'
  # dt$activity[dt$deltaBitDepthSmooth>prm.t$deltaHoleBitZeroTolerance] <- 'Drilling'
  
  # Activity automatically set to match any tripping rig state classifications
  dt$activity[dt[[prm.t$target]]=='TrippingIn' | dt[[prm.t$target]]=='TripInConnect'] <- 'TrippingIn'
  dt$activity[dt[[prm.t$target]]=='TrippingOut' | dt[[prm.t$target]]=='TripOutConnect'] <- 'TrippingOut'

  # Initial activities
  ra <- data.frame(values=rle(as.vector(dt$activity))$values,
                   lengths=rle(as.vector(dt$activity))$lengths)
  cat('\nInitial activity classification run count=',nrow(ra),' classification observation counts:')
  print(table(dt$activity))

  # Agglomerate any short duration classifications
  if (nrow(ra)>1) {
    ra$values <- as.character(ra$values)
    ra$from <- 0
    ra$to <- 0
    ra$to <- cumsum(ra$lengths)
    ra$from <- ra$to - ra$lengths + 1
    agglomCount <- 0  # Count of agglomerated activity intervals
    agglomObCount <- 0 # Count of agglomerated individual observations
    # Loop through the activity intervals from latest to earliest 
    for (i in (nrow(ra)-1):1) {
      if (ra$lengths[i]<prm.rc$agglomerateCount) { # if current interval is short
        # If subsequent interval is tripping and current interval is not tripping
        if ((ra$values[i+1]=='TrippingIn' | ra$values[i+1]=='TrippingOut') |
            (ra$values[i] != 'TrippingIn' & ra$values[i] != 'TrippingOut')) {
          # assign activity to corresponding tripping activity at both the interval (ra) and observation (dt) levels
          ra$values[i] <- ra$values[i+1]
          dt$activity[ra$from[i]:ra$to[i]] <- ra$values[i+1]
          # update counters
          agglomCount <- agglomCount+1
          agglomObCount <- agglomObCount + ra$to[i] - ra$from[i] + 1
        }
      } 
    }
    cat('\nactivity consolidation: agglomerated ', agglomCount,' runs and ',agglomObCount,
        '(',round(100*agglomObCount/nrow(dt),digits=1),'%) of observations with run lengths less than',prm.rc$agglomerateCount)
  }
  
  # Correct post-agglomeration misclassifications
  # re-establish post agglomeration activity intervals
  ra <- data.frame(values=rle(as.vector(dt$activity))$values,
                   lengths=rle(as.vector(dt$activity))$lengths)
  if(nrow(ra)>1) {
    ra$values <- as.character(ra$values)
    ra$from <- 0
    ra$to <- 0
    ra$to <- cumsum(ra$lengths)
    ra$from <- ra$to - ra$lengths + 1
    correctionCount <- 0 # Count of corrected activity intervals
    correctionObCount <- 0 # Count of corrected activity observations
    # Loop through the activity intervals earliest to latest
    for (i in 1:nrow(ra)) {
      if (ra$values[i]=='NotDrilling') {
        if (!is.na(dt$EDR_HoleDepth_runmean[ra$from[i]]) & !is.na(dt$EDR_HoleDepth_runmean[ra$to[i]])) {
          # If NotDrilling activity has positive holedepth progress, it is reclassified as Drilling
          if (dt$EDR_HoleDepth_runmean[ra$to[i]] > (dt$EDR_HoleDepth_runmean[ra$from[i]]+prm.t$deltaHoleBitZeroTolerance*(ra$to[i]-ra$from[i]+1))) {
            ra$values[i] <- 'Drilling'
            dt$activity[ra$from[i]:ra$to[i]] <- 'Drilling'
            correctionCount <- correctionCount+1
            correctionObCount <- correctionObCount + ra$to[i] - ra$from[i] + 1
          }
        }
      }
      if (ra$values[i]=='NotDrilling') {
        if (!is.na(dt$EDR_BitDepth_runmean[ra$from[i]]) & !is.na(dt$EDR_BitDepth_runmean[ra$to[i]])) {
          # If NotDrilling activity has positive bitdepth progress, it is reclassified as TrippingIn
          if (dt$EDR_BitDepth_runmean[ra$to[i]] > (dt$EDR_BitDepth_runmean[ra$from[i]]+prm.t$deltaHoleBitZeroTolerance*(ra$to[i]-ra$from[i]+1))) {
            ra$values[i] <- 'TrippingIn'
            dt$activity[ra$from[i]:ra$to[i]] <- 'TrippingIn'
            correctionCount <- correctionCount+1
            correctionObCount <- correctionObCount + ra$to[i] - ra$from[i] + 1
          # If NotDrilling activity has negative bitdepth progress, it is reclassified as TrippingOut
          } else if (dt$EDR_BitDepth_runmean[ra$to[i]] < (dt$EDR_BitDepth_runmean[ra$from[i]]-prm.t$deltaHoleBitZeroTolerance*(ra$to[i]-ra$from[i]+1))) {
            ra$values[i] <- 'TrippingOut'
            dt$activity[ra$from[i]:ra$to[i]] <- 'TrippingOut'
            correctionCount <- correctionCount+1
            correctionObCount <- correctionObCount + ra$to[i] - ra$from[i] + 1
          }
        }
      }
    }
    cat('\nactivity consolidation: corrected ', correctionCount,' runs and ',correctionObCount,
        '(',round(100*correctionObCount/nrow(dt),digits=1),'%) of observations with incorrect average activity classifications\n')
  }
  
  # Insure that activity interval edges match corresponding rig state contiguous runs for 
  # Connecting, TrippingIn, and TrippingOut, FHS Sept 16, 2016
  
  # ra: activity intervals
  ra <- data.frame(values=rle(as.vector(dt$activity))$values,
                   lengths=rle(as.vector(dt$activity))$lengths)
  ra$values <- as.character(ra$values)
  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1
  
  # rr: rig state classification intervals
  rr <- data.frame(values=rle(as.vector(dt$rig_state))$values,
                   lengths=rle(as.vector(dt$rig_state))$lengths)
  rr$values <- as.character(rr$values)
  rr$from <- 0
  rr$to <- 0
  rr$to <- cumsum(rr$lengths)
  rr$from <- rr$to - rr$lengths + 1
  
  trippingInCount <- 0 # Count of activity observations converted to TrippingIn to match rig state classification
  trippingOutCount <- 0 # Count of activity observations converted to TrippingOut to match rig state classification
  drillingCount <- 0 # Count of activity observations converted to Drilling to match Connecting rig state classification
  
  # # More condensed code to determine activity extensions, FHS Oct 21, 2018
  # extendActivity <- data.frame(rig_state=c('TrippingIn','TripInConnect','TrippingOut','TripOutConnect','Connecting'),
  #                              extend_activity=c('TrippingIn','TrippingIn','TrippingOut','TrippingOut','Drilling'),
  #                              extend_count=c(0,0,0,0,0))
  # for (i in 1:nrow(ra)) {
  #   # checks which extend rig_state candidate (if any) is present
  #   # note that j can point to multiple identical extend_activity values (i.e. length(j) can be greater than one!
  #   j <- which(extendActivity$extend_activity %in% ra$values[i]) 
  #   if (length(j)>0) { # At least one extend activity candidate has been found
  #     # index the rig state interval which could straddle the start of the current activity interval
  #     rr_index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
  #     # see which (if any) of the extend activity rig state candidates match the rr_index rig_state
  #     k <- which(extendActivity$rig_state[j] %in% rr$values[rr_index])
  #     # if there is a match and the rig state interval straddles the boundary of the activity interval
  #     # then the activity interval is extended
  #     if(length(k)>0 & rr$from[rr_index] < ra$from[i]) {
  #       # set j to the single matching rig state
  #       j <- j[k]
  #       # modify the activity
  #       dt$activity[rr$from[rr_index]:ra$from[i]] <- extendActivity$extend_activity[j]
  #       #increment the counter
  #       extendActivity$extend_count <- extendActivity$extend_count + ra$from[i] - rr$from[index]
  #     } 
  #   }
  # }
  

  for (i in 1:nrow(ra)) {
    if (ra$values[i]=='TrippingIn') {
      # index is the rig state interval which could straddle the start of the current activity interval
      index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
      # if the rig state is 'TrippingIn' and it extends before the start of current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TrippingIn' & rr$from[index] < ra$from[i]) {
#         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
#             ' down to rr$from[index]=',rr$from[index],' to match rig states.')
        dt$activity[rr$from[index]:ra$from[i]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + ra$from[i] - rr$from[index]
      }
      # index is the rig state interval which could straddle the end of the current activity interval
      index <- which(rr$from<=ra$to[i] & rr$to>=ra$to[i])[1]
      # if the rig state is 'TrippingIn' and it extends after the end current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TrippingIn' & rr$to[index] > ra$to[i]) {
#         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
#             ' to rr$to[index]=',rr$to[index],' to match rig states.')
        dt$activity[ra$to[i]:rr$to[index]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + rr$to[index] - ra$to[i]
      }
    }
    # Same logic used for 'TripInConnect' as 'TrippingIn' above
    if (ra$values[i]=='TrippingIn') {
      # index is the rig state interval which could straddle the start of the current activity interval
      index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
      # if the rig state is 'TripInConnect' and it extends before the start of current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TripInConnect' & rr$from[index] < ra$from[i]) {
        #         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
        #             ' down to rr$from[index]=',rr$from[index],' to match rig states.')
        dt$activity[rr$from[index]:ra$from[i]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + ra$from[i] - rr$from[index]
      }
      # index is the rig state interval which could straddle the end of the current activity interval
      index <- which(rr$from<=ra$to[i] & rr$to>=ra$to[i])[1]
      # if the rig state is 'TripInConnect' and it extends after the end current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TripInConnect' & rr$to[index] > ra$to[i]) {
        #         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
        #             ' to rr$to[index]=',rr$to[index],' to match rig states.')
        dt$activity[ra$to[i]:rr$to[index]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + rr$to[index] - ra$to[i]
      }
    }
    # Same logic used for 'TrippingOut' as 'TrippingIn' above
    if (ra$values[i]=='TrippingOut') {
      index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
      if (rr$values[index]=='TrippingOut' & rr$from[index] < ra$from[i]) {
#         cat('\nExtending TrippingOut activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
#             ' down to rr$from[index]=',rr$from[index],' to match rig states.')
        dt$activity[rr$from[index]:ra$from[i]] <- 'TrippingOut'
        trippingOutCount <- trippingOutCount + ra$from[i] - rr$from[index]
      }
      index <- which(rr$from<=ra$to[i] & rr$to>=ra$to[i])[1]
      if (rr$values[index]=='TrippingOut' & rr$to[index] > ra$to[i]) {
#         cat('\nExtending TrippingOut activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
#             ' to rr$to[index]=',rr$to[index],' to match rig states.')
        dt$activity[ra$to[i]:rr$to[index]] <- 'TrippingOut'
        trippingOutCount <- trippingOutCount + rr$to[index] - ra$to[i]
      }
    }
    # Same logic used for 'TripOutConnect' as 'TrippingIn' above
    if (ra$values[i]=='TrippingOut') {
      # index is the rig state interval which could straddle the start of the current activity interval
      index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
      # if the rig state is 'TripOutConnect' and it extends before the start of current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TripOutConnect' & rr$from[index] < ra$from[i]) {
        #         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
        #             ' down to rr$from[index]=',rr$from[index],' to match rig states.')
        dt$activity[rr$from[index]:ra$from[i]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + ra$from[i] - rr$from[index]
      }
      # index is the rig state interval which could straddle the end of the current activity interval
      index <- which(rr$from<=ra$to[i] & rr$to>=ra$to[i])[1]
      # if the rig state is 'TripOutConnect' and it extends after the end current activity interval
      # then the activity interval is extended
      if (rr$values[index]=='TripOutConnect' & rr$to[index] > ra$to[i]) {
        #         cat('\nExtending TrippingIn activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
        #             ' to rr$to[index]=',rr$to[index],' to match rig states.')
        dt$activity[ra$to[i]:rr$to[index]] <- 'TrippingIn'
        trippingInCount <- trippingInCount + rr$to[index] - ra$to[i]
      }
    }
    # Same logic used for 'Drilling'(and 'Connecting') as 'TrippingIn' above
    if (ra$values[i]=='Drilling') {
      index <- which(rr$from<=ra$from[i] & rr$to>=ra$from[i])[1]
      if (rr$values[index]=='Connecting' & rr$from[index] < ra$from[i]) {
#         cat('\nExtending Drilling activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
#             ' down to rr$from[index]=',rr$from[index],' to match rig states.')
        dt$activity[rr$from[index]:ra$from[i]] <- 'Drilling'
        drillingCount <- drillingCount + ra$from[i] - rr$from[index]
      }
      index <- which(rr$from<=ra$to[i] & rr$to>=ra$to[i])[1]
      if (rr$values[index]=='Connecting' & rr$to[index] > ra$to[i]) {
#         cat('\nExtending Drilling activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
#             ' to rr$to[index]=',rr$to[index],' to match rig states.')
        dt$activity[ra$to[i]:rr$to[index]] <- 'Drilling'
        drillingCount <- drillingCount + rr$to[index] - ra$to[i]
      }
    }
  }
    
  # Now look at 'Drilling' activity interval extensions to cover possible WtoW connect times
  # Reinitialize activity intervals with any previous extensions
  # ra: activity intervals
  ra <- data.frame(values=rle(as.vector(dt$activity))$values,
                   lengths=rle(as.vector(dt$activity))$lengths)
  ra$values <- as.character(ra$values)
  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1
  # logic test for WtoW connect times extension test
  #    EDR_ROP=zero and EDR_HoleDepth_delta=0 and non-tripping rig state
  dt$WtoWextend <- dt$EDR_ROP < prm.t$deltaHoleBitZeroTolerance &
    dt$EDR_HoleDepth_delta < prm.t$deltaHoleBitZeroTolerance &
    dt[[prm.t$target]] != 'RDrilling' &
    dt[[prm.t$target]] != 'SDrilling' &
    dt[[prm.t$target]] != 'TrippingIn' &
    dt[[prm.t$target]] != 'TripOutConnect' &
    dt[[prm.t$target]] != 'TrippingOut' & 
    dt[[prm.t$target]] != 'TripOutConnect'
  dt$WtoWextend[is.na(dt$WtoWextend)] <- FALSE
  # re: WtoW Drilling Extension Candidate Intervals
  re <- data.frame(values=rle(as.vector(dt$WtoWextend))$values,
                   lengths=rle(as.vector(dt$WtoWextend))$lengths)
  re$values <- as.character(re$values)
  re$from <- 0
  re$to <- 0
  re$to <- cumsum(re$lengths)
  re$from <- re$to - re$lengths + 1
  drillingWtoWCount <- 0 # Count of activity observations converted to Drilling to cover WtoW connect time candidates
  for (i in 1:nrow(ra)) {
    if (ra$values[i]=='Drilling') {
      # index is the WtoW extension candidate interval which could straddle the start of the current activity interval
      index <- which(re$from<=ra$from[i] & re$to>=ra$from[i])[1]
      # if the WtoW extension candidate interval is TRUE and it extends before the activity interval 
      # then the activity interval is extended
      if (re$values[index]==TRUE & re$from[index] < ra$from[i]) {
#         cat('\nExtending Drilling activity interval i=',i,' index=',index,' from ra$from[i]=',ra$from[i],
#              ' down to re$from[index]=',re$from[index],' for WtoW candidates.')
        dt$activity[re$from[index]:ra$from[i]] <- 'Drilling'
        drillingWtoWCount <- drillingWtoWCount + ra$from[i] - re$from[index]
      }
      index <- which(re$from<=ra$to[i] & re$to>=ra$to[i])[1]
      if (re$values[index]==TRUE & re$to[index] > ra$to[i]) {
#         cat('\nExtending Drilling activity interval i=',i,' index=',index,' from ra$to[i]=',ra$to[i],
#            ' to re$to[index]=',re$to[index],' for WtoW candidates.')
        dt$activity[ra$to[i]:re$to[index]] <- 'Drilling'
        drillingWtoWCount <- drillingWtoWCount + re$to[index] - ra$to[i]
      }
    }
  }
  cat('\nactivity observation conversions for match to rig state contiguous run edges')
  cat('\nTrippingIn count=',trippingInCount,' TrippingOut count=',trippingOutCount,
      '\nConnecting count=',drillingCount,' Connecting WtoW count=',drillingWtoWCount,'\n')
  
  # Final classifications
  ra <- data.frame(values=rle(as.vector(dt$activity))$values,
                   lengths=rle(as.vector(dt$activity))$lengths)
  
  dt$activity <- factor(dt$activity,levels=c("Drilling","NotDrilling","TrippingIn","TrippingOut"))
  
  cat('\nFinal activity classification run count=',nrow(ra),' classification observation counts:')
  print(table(dt$activity))
  
  return(dt)
}