##########################################################################
# performancereports.R - rig state based performance reports
# Average connection time during drilling, tripping in & out speeds
# Ensign Energy Services Inc. retains all rights to this software
# FHS Nov 8, 2018
##########################################################################
performancereports <- function(dt,prm.t,prm.rc) {
  Epsilon <- 1e-5
  
  # Set ID to rowcount, then collapse out all NAs.
  # Can reconstruct NA count for interval by deltaID minus row count
  dt$ID <- 1:nrow(dt)
  
  # Compute deltaBlockHeight, deltaHoleDepth, deltaBitDepth
  dt$deltaBlockHeight <- 0
  dt$deltaBlockHeight[2:nrow(dt)] <- dt$EDR_BlockHeight[2:nrow(dt)] - dt$EDR_BlockHeight[1:(nrow(dt)-1)]
  dt$deltaBitDepth[2:nrow(dt)] <- dt$EDR_BitDepth[2:nrow(dt)] - dt$EDR_BitDepth[1:(nrow(dt)-1)]
  dt$deltaHoleDepth[2:nrow(dt)] <- dt$EDR_HoleDepth[2:nrow(dt)] - dt$EDR_HoleDepth[1:(nrow(dt)-1)]
  
  dt <- dt[!(is.na(dt$EDR_ROP)),]
  dt <- dt[!(is.na(dt$EDR_HoleDepth)),]
  dt <- dt[!(is.na(dt$EDR_BitDepth)),]
  dt <- dt[!(is.na(dt[[prm.t$target]])),]
  dt <- dt[!(is.na(dt$deltaBlockHeight)),]
  dt <- dt[!(is.na(dt$deltaBitDepth)),]
  dt <- dt[!(is.na(dt$deltaHoleDepth)),]

  # Assumes that prm.rc thresholds are metric
  # If length_units are in feet, then makes conversion
  if (!(is.null(dt$BitDepth_UOM))) {
    if (length(which(dt$BitDepth_UOM=='feet'))>0) {
      cat('\nPerformance reporting lengths in feet\n')
      prm.rc$minDeltaHoleDepth <- round(prm.rc$minDeltaHoleDepth/0.3048,digits=1)
      prm.rc$bitDepthThreshold <- round(prm.rc$bitDepthThreshold/0.3048,digits=1)
      prm.t$maxDeltaHoledepthRate <- round(prm.t$maxDeltaHoledepthRate/0.3048,digits=1)
    }
  }
  cat('\nPerformance Report listing of activity categories table(dt$activity)')
  print(table(dt$activity))
  cat('\nPerformance Report listing of rig states by activity categories table(dt$rig_state,dt$activity)')
  print(table(dt$rig_state,dt$activity))
  # If options selected, join dt$activity and dt$Driller into dt$activityDriller so that
  # performance analysis can be performed on individual drillers
  if (prm.rc$singleDrillerReporting==TRUE) {
    if (is.null(dt$Driller)) dt$Driller <- 'UNK'
    dt$Driller <- as.character(dt$Driller)
    dt$Driller[is.na(dt$Driller)] <- 'UNK'
    if (is.null(dt$IsBitInCasing)) dt$IsBitInCasing <- 'UNK'
    dt$IsBitInCasing <- as.character(dt$IsBitInCasing)
    dt$IsBitInCasing[is.na(dt$IsBitInCasing)] <- 'UNK'
    if (is.null(dt$IsCrewHandlingBHA)) dt$IsCrewHandlingBHA <- 'UNK'
    dt$IsCrewHandlingBHA <- as.character(dt$IsCrewHandlingBHA)
    dt$IsCrewHandlingBHA[is.na(dt$IsCrewHandlingBHA)] <- 'UNK'
    
    # Always subdivide by individual drillers
    dt$activityDriller <- paste0(as.character(dt$activity),dt$Driller)
    
    # Only do InCasing and BHA subdivision breakout for Tripping activities
    select <- dt$activity=='TrippingIn' | dt$activity=='TrippingOut'
    dt$activityDriller[select] <- paste0(dt$activityDriller[select],dt$IsBitInCasing[select],dt$IsCrewHandlingBHA[select])
#     cat('\nTable of dt$activityDriller\n')
#     print(table(dt$activityDriller,dt$activity))
  } else {
    dt$activityDriller <- dt$activity
  }

  ######################################################################################
  # Create Drilling Connecting Time Report
  
  repconnect <- data.frame(Rig=NULL,activity=NULL, FromDatetime=NULL, ToDatetime=NULL,
                           FromDepth=NULL, ToDepth=NULL, NumberOfConnections=NULL, 
                           AvgWtoWConnectTime=NULL,AvgReamCCTime=NULL,AvgStoSConnectTime=NULL,AvgSurveyTime=NULL,
                           StoSRFVote=NULL,Missing=NULL,Drillers=NULL,Remark=NULL)
  
  repconnectdetail <- data.frame(WellId=NULL,Rig=NULL,DrillerName=NULL,DateTimeStart=NULL,
                                 ReamCCTime=NULL,StoSConnectTime=NULL,SurveyTime=NULL,WtoWConnectTime=NULL)
  
  # Look for instances where driller name or other splits connecting event
  # If this occurs, agglomerate into single connecting event, Oct 24, 2017
  ra <- data.frame(values=rle(as.vector(dt$activityDriller))$values,
                   lengths=rle(as.vector(dt$activityDriller))$lengths)
  ra$values <- as.character(ra$values)
  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1
  if (nrow(ra)>1) { # insure that ra has a least 2 rows before continuing, MJK 2018-04-09
    for (i in 1:(nrow(ra)-1)) {
      if (substring(ra$values[i],1,8)=="Drilling" & substring(ra$values[(i+1)],1,8)=="Drilling" &
          dt[[prm.t$target]][ra$to[i]]=='Connecting' & dt[[prm.t$target]][ra$from[(i+1)]]=='Connecting') {
        cat('\n\nperformance report WARNING... found connecting event split between two drilling activities at ',dt$time[ra$to[i]],'\n')
        # Agglomerate records from overlap onto preceding activityDriller
        j1 <- ra$from[(i+1)] # start of agglomeration
        # j2 connecting intervals to be agglomerated
        j2 <- j1 + min((ra$to[(i+1)]-j1+1),which(dt[[prm.t$target]][j1:ra$to[(i+1)]] != 'Connecting')) - 1 # j2 'Connecting' intervals to be converted
        # j3 survey time with zero RP and zero deltaHoleDepth values to be agglomerated 
        j3 <- j2 + min((ra$to[(i+1)]-j2+1),which(dt$EDR_ROP[j2:ra$to[(i+1)]]>Epsilon |
                        dt$deltaHoleDepth[j2:ra$to[(i+1)]]>Epsilon)) - 1
        dt$activityDriller[j1:j3] <- ra$values[i]
      }
    }
  }

  # activity Runs - looking for Drilling activity
  ra <- data.frame(values=rle(as.vector(dt$activityDriller))$values,
                   lengths=rle(as.vector(dt$activityDriller))$lengths)
  ra$values <- as.character(ra$values)

  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1
  minDeltaHoleDepthDropCount <- 0 # Count of dropped connection intervals from low delta hole depth
  minStoSConnectTimeDropCount <- 0 # Count of dropped connection intervals from low StoS Connect time
  maxDeltaHoleDepthRateDropCount <- 0 # Count of dropped connnection intervals from high holedepth rate (holedepth instrument resets)

  ra$values[substring(ra$values,1,8)=="Drilling"] <- "Drilling"
  ra$values[substring(ra$values,1,11)=="NotDrilling"] <- "NotDrilling"
  ra$values[substring(ra$values,1,10)=="TrippingIn"] <- "TrippingIn"
  ra$values[substring(ra$values,1,11)=="TrippingOut"] <- "TrippingOut"
  
  for (i in 1:nrow(ra)) {
    if (ra$values[i]=='Drilling') { 
      # Determine average connect times for this 'Drilling' activity period
      temp <- dt[ra$from[i]:ra$to[i],]
      # Setup connectEvent to track each connecting event separately
      connectEvent <- data.frame(from=NULL,  # index for first WtoW record in temp
                                 to=NULL,    # index for last WtoW record in temp
                                 WtoWConnectTime=NULL,  # timespan
                                 ReamCCTime=NULL,       # timespan
                                 StoSConnectTime=NULL,  # timespan
                                 SurveyTime=NULL,       # timespan
                                 StoSRFVote=NULL,       # rfvotes weighted by timespan
                                 Missing=NULL)          # count of no rig state observations between from and to

      # deltaHoleDepth to trigger warning if negative values (unreal) values are encountered 
      temp$deltaHoleDepth <- 0
      if (nrow(temp)>1) {
        temp$deltaHoleDepth[2:nrow(temp)] <- temp$EDR_HoleDepth[2:nrow(temp)] - 
                                             temp$EDR_HoleDepth[1:(nrow(temp)-1)]
      } 
      
      temp$WtoWtimespan <- 0 # Initialize timespans for WtoW to zero
      temp$ReamCCtimespan <- 0 # Initialize timespans for ReamCC to zero
      temp$Surveytimespan <- 0 # Initialize timespans for Survey to zero
      rrs <- data.frame(values=rle(as.vector(temp[[prm.t$target]]))$values,
                        lengths=rle(as.vector(temp[[prm.t$target]]))$lengths)
      rrs$from <- 0
      rrs$to <- 0
      rrs$to <- cumsum(rrs$lengths)
      rrs$from <- rrs$to - rrs$lengths + 1
      # compute timespan and rfvote for each rrs record
      rrs$timespan <- NA 
      rrs$rfvote <- NA

      rrs$values <- as.character(rrs$values)
#       cat('\n\nperformancereports DIAG A Drilling i=',i,' ra$from[i]=',ra$from[i],' ra$to[i]=',ra$to[i],
#           " sum(rrs$values=='Connecting')=",sum(rrs$values=='Connecting'),' sum(is.na(temp$timespan))=',sum(is.na(temp$timespan)))
      for (j in which(!(is.na(rrs$values)))) {
        rrs$timespan[j] <- sum(temp$timespan[rrs$from[j]:rrs$to[j]])
        rrs$rfvote[j] <- sum(temp$timespan[rrs$from[j]:rrs$to[j]]*temp$rfvote[rrs$from[j]:rrs$to[j]])
      }

      for (j in which(!(is.na(rrs$values)))) {
        # Test that there is at least 1 'RDrilling' or 'SDrilling' interval preceding the 'Connecting' event (Aug 25, 2017)
        if (rrs$values[j]=='Connecting') {
          count <- length(which(dt[[prm.t$target]][1:ra$from[i]]=='RDrilling' | dt[[prm.t$target]][1:ra$from[i]]=='SDrilling')) +
            length(which(temp[[prm.t$target]][1:rrs$from[j]]=='RDrilling' | temp[[prm.t$target]][1:rrs$from[j]]=='SDrilling'))
          if (count<1) rrs$values[j]='Other'
        }
        
        if (rrs$values[j]=="Connecting") {
          if (j<nrow(rrs)) {
            # lookup next "Connecting"  event in rrs (j2)
            j2 <- if (length(which(rrs$values[(j+1):nrow(rrs)]=="Connecting"))>0) 
              min(which(rrs$values[(j+1):nrow(rrs)]=="Connecting")+j) else j
            # check that deltaHoleDepth from current to next "Connecting" is at least prm.rc$minDeltaHoleDepth
            if ((temp$EDR_HoleDepth[rrs$from[j2]] - temp$EDR_HoleDepth[rrs$to[j]])<prm.rc$minDeltaHoleDepth &
                j2>j) {
              minDeltaHoleDepthDropCount <- minDeltaHoleDepthDropCount + 1
              rrs$values[j] <- "Other"
            }
            # check that max holedepth rate from current to next "Connecting" event is less than prm.t$maxDeltaHoledepthRate
            # This avoids counting hole depth instrument resets as drilling activities
            if (max(temp$deltaHoleDepth[j:j2]/temp$timespan[j:j2])>prm.t$maxDeltaHoledepthRate & j2>j) {
              maxDeltaHoleDepthRateDropCount <- maxDeltaHoleDepthRateDropCount + 1
              rrs$values[j] <- "Other"
            }
          }
        }
        
        # If rrs$values[i]=='Connecting' then determine the expanded timespan
        # on either side of 'Connecting' where temp$ROP < Epsilon
        # This corresponds to the timespan for the WtoW Connecting activity
        if (rrs$values[j]=="Connecting" & rrs$timespan[j]/60 >= prm.rc$minStoSConnectTime) {
          # We have a "Connecting" event, so initialize a new record
          connectEvent <- rbind(connectEvent,data.frame(from=0,
                                                        to=0,
                                                        WtoWConnectTime=0,
                                                        ReamCCTime=0,
                                                        StoSConnectTime=0,
                                                        SurveyTime=0,
                                                        StoSRFVote=0,
                                                        Missing=0))
          if (rrs$from[j]>1) { # There are some preceding values to consider
            if (sum(which(temp$EDR_ROP[1:(rrs$from[j]-1)]>Epsilon | 
                          temp$deltaHoleDepth[1:rrs$from[j]-1]>Epsilon))>0) { 
              # the preceding values have some positive ROP or deltaHoleDepth values
              # the max is last positive value, so add 1 for first non-positive value (FHS Nov 3, 2017)
              j1 <- max(which(temp$EDR_ROP[1:(rrs$from[j]-1)]>Epsilon | 
                              temp$deltaHoleDepth[1:rrs$from[j]-1]>Epsilon)) + 1
            } else {
              j1 <- 1 # No positive previous ROP values so WtoW index goes to 1 
            }
          } else {
            j1 <- rrs$from[j] # No preceding values so WtoW lower index same as StoS
          }
          if (rrs$to[j]<nrow(temp)) { # There are some successor values
            if (sum(which(temp$EDR_ROP[(rrs$to[j]+1):nrow(temp)]>Epsilon |
                          temp$deltaHoleDepth[(rrs$to[j]+1):nrow(temp)]>Epsilon))>0) { 
              # successor values have some positive ROP or deltaHoleDepth values
              # the min is the first positive value, so substract 1 for last non-positive value (FHS Nov 3, 2017) 
              j2 <- min(which(temp$EDR_ROP[(rrs$to[j]+1):nrow(temp)]>Epsilon |
                              temp$deltaHoleDepth[(rrs$to[j]+1):nrow(temp)]>Epsilon))+rrs$to[j] - 1
            } else {
              j2 <- nrow(temp) # No positive ROP values so WtoW index goes to nrow(temp) 
            }
          } else {
            j2 <- rrs$to[j] # No successor values so WtoW lower index same as StoS
          }
#           cat('\nperformancereports DIAG C i=',i,' j=',j,' j1=',j1,' rrs$from[j]=',rrs$from[j],' rrs$to[j]=',rrs$to[j],' j2=',j2,
#               ' connect time from=',temp$time[rrs$from[j]])
#           cat('\n   from=',temp$time[j1],' to=',temp$time[j2],' adding ',sum(temp$timespan[j1:j2]),' to WtoW timespan')
          
          connectEvent$from[nrow(connectEvent)] <- j1
          connectEvent$to[nrow(connectEvent)] <- j2
          connectEvent$WtoWConnectTime[nrow(connectEvent)] <- sum(temp$timespan[j1:j2],na.rm=T)
          connectEvent$StoSConnectTime[nrow(connectEvent)] <- sum(temp$timespan[rrs$from[j]:rrs$to[j]])
          connectEvent$StoSRFVote[nrow(connectEvent)] <- rrs$rfvote[j]
          connectEvent$Missing[nrow(connectEvent)] <- (temp$ID[j2] - temp$ID[j1]) - (j2 - j1) 

          temp$WtoWtimespan[j1:j2] <- temp$timespan[j1:j2]
          if (j1<rrs$from[j]) {
#             cat('\nDIAG1 ReamCC j1=',j1,' rrs$from[j]=',rrs$from[j],' sum(temp$timespan[j1:(rrs$from[j]-1)])=',
#                 round(sum(temp$timespan[j1:(rrs$from[j]-1)])/60,digits=2),' minutes')
            temp$ReamCCtimespan[j1:(rrs$from[j]-1)] <- temp$timespan[j1:(rrs$from[j]-1)]
            connectEvent$ReamCCTime[nrow(connectEvent)] <- sum(temp$timespan[j1:(rrs$from[j]-1)],na.rm=T)
          }
          if (j2>rrs$to[j]) {
            temp$Surveytimespan[(rrs$to[j]+1):j2] <- temp$timespan[(rrs$to[j]+1):j2]
            connectEvent$SurveyTime[nrow(connectEvent)] <- sum(temp$timespan[(rrs$to[j]+1):j2],na.rm=T)
          }
        } else if (rrs$values[j]=="Connecting" & rrs$timespan[j]/60 < prm.rc$minStoSConnectTime) { 
#           cat('\nDIAG3 Connect event rejected because StoS interval=',rrs$timespan[j]/60,
#               ' too small rrs$from[j]=',rrs$from[j],' rrs$to[j]=',rrs$to[j])
          minStoSConnectTimeDropCount <- minStoSConnectTimeDropCount + 1
        }
      } # for (j in which(!(is.na(rrs$values))))

      if((max(temp$EDR_HoleDepth) - min(temp$EDR_HoleDepth))>prm.rc$minDeltaHoleDepth*max(1,sum(rrs$values=='Connecting'))) {
        if (sum(temp[[prm.t$target]]=='Connecting')>0 & nrow(connectEvent)>0) {
          fromDatetimeIndex <- min(max(which(temp$EDR_HoleDepth==min(temp$EDR_HoleDepth))),
                                   min(which(temp$WtoWtimespan>0)))
          if (fromDatetimeIndex>1) {
            fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex]))+
                                                     as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex-1])))/2,
                                                     origin="1970-01-01 00:00:00"))
          } else {
            # At the beginning of the activity interval with fromDatetimeIndex=1, 
            # so look at time halfway to previous observation before current activity
            if(ra$from[i]>1) {
              fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$from[i]]))+
                                                         as.numeric(as.POSIXlt(dt$time[ra$from[i]-1])))/2,
                                                      origin="1970-01-01 00:00:00"))
            } else {
              # First observation in dt data.frame, so extrapolate half timespan
              fromDatetime <- as.character(as.POSIXlt(temp$time[fromDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")-
                                             temp$timespan[fromDatetimeIndex]/2)
            }
          }
          toDatetimeIndex <- max(min(which(temp$EDR_HoleDepth==max(temp$EDR_HoleDepth))),
                                 max(which(temp$WtoWtimespan>0)))
          if (toDatetimeIndex<nrow(temp)) {
            toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[toDatetimeIndex]))+
                                                   as.numeric(as.POSIXlt(temp$time[toDatetimeIndex+1])))/2,
                                                   origin="1970-01-01 00:00:00"))
          } else {
            # At the end of the activity interval with toDatetimeIndex=nrow(temp), 
            # so look at time halfway to next observation after current activity
            if (ra$to[i]<nrow(dt)) {
              toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$to[i]]))+
                                                       as.numeric(as.POSIXlt(dt$time[ra$to[i]+1])))/2,
                                                    origin="1970-01-01 00:00:00"))
            } else {
              # Last observation in dt data.frame, so extrapolate half timespan
              toDatetime <- as.character(as.POSIXlt(temp$time[toDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")+
                                           temp$timespan[toDatetimeIndex]/2)
            }
          }
        } else { # There are no connecting events...sum(temp[[prm.t$target]]=='Connecting')==0
          fromDatetime <- NA
          toDatetime <- NA
        }
        if (sum(rrs$values=='Connecting')>0 & nrow(connectEvent)>0) {
          
          # Put loop in here for possible connectEvent breakouts... FHS May 13, 2017
          minFromDatetime <- fromDatetime
          maxToDatetime <- toDatetime
          minFromDatetimeIndex <- fromDatetimeIndex
          maxToDatetimeIndex <- toDatetimeIndex
          
          c1 <- 1
          for (c in 1:nrow(connectEvent)) {
            # Create new record for repconnectdetail
            repconnectdetail <- rbind(repconnectdetail,data.frame(
               WellId=if(is.null(temp$WellName)) 'NONE' else temp$WellName[connectEvent$from[c]],
               Rig=temp$Rig[connectEvent$from[c]],
               DrillerName=if (prm.rc$singleDrillerReporting) temp$Driller[connectEvent$from[c]] else 
                 paste(as.character(unique(temp$Driller[connectEvent$from[c]:connectEvent$to[c]])),collapse=";"),
               DateTimeStart=if(connectEvent$from[c]>1) as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[connectEvent$from[c]]))+
                                                        as.numeric(as.POSIXlt(temp$time[connectEvent$from[c]-1])))/2,
                                                        origin="1970-01-01 00:00:00")) else minFromDatetime,
               ReamCCTime=round(connectEvent$ReamCCTime[c]/60,digits=1),
               StoSConnectTime=round(connectEvent$StoSConnectTime[c]/60,digits=1),
               SurveyTime=round(connectEvent$SurveyTime[c]/60,digits=1),
               WtoWConnectTime=round(connectEvent$WtoWConnectTime[c]/60,digits=1)
            ))
            
            c2 <- c
            # Check for change in ReamCCTime excessive time status
            # If status changes in next record, then create new record
            if (c < nrow(connectEvent)) {
              if ((connectEvent$ReamCCTime[c]/60 < prm.rc$ReamCCTime &
                   connectEvent$ReamCCTime[c+1]/60 >= prm.rc$ReamCCTime) |
                  connectEvent$ReamCCTime[c]/60 >= prm.rc$ReamCCTime) flag <- TRUE else flag <- FALSE
#                   (connectEvent$ReamCCTime[c]/60 >= prm.rc$ReamCCTime &
#                    connectEvent$ReamCCTime[c+1]/60 < prm.rc$ReamCCTime)) flag <- TRUE else flag <- FALSE
            } else {
              flag <- TRUE # c==nrow(connectEvent) last connect event
            }
            
            if (flag) {
              if (c1==1) {
                fromDatetime <- minFromDatetime
                fromDatetimeIndex <- minFromDatetimeIndex
                n1 <- 1
              } else {
                fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[connectEvent$from[c1]]))+
                                                         as.numeric(as.POSIXlt(temp$time[connectEvent$from[c1]-1])))/2,
                                                        origin="1970-01-01 00:00:00"))
                fromDatetimeIndex <- connectEvent$from[c1]
                n1 <- connectEvent$from[c1]
              }
              if (c2==nrow(connectEvent)) {
                toDatetime <- maxToDatetime
                toDatetimeIndex <- maxToDatetimeIndex
                # cat('\nDIAG2 setting n2 to nrow(temp)=',nrow(temp))
                n2 <- nrow(temp)
              } else {
                toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[connectEvent$to[c2]]))+
                                                       as.numeric(as.POSIXlt(temp$time[connectEvent$to[c2]+1])))/2,
                                                        origin="1970-01-01 00:00:00"))
                toDatetimeIndex <- connectEvent$to[c2]
                # cat('\nDIAG3 setting n2 to connectEvent$to[c2]=',connectEvent$to[c2],' nrow(connectEvent)=',nrow(connectEvent))
                n2 <- connectEvent$to[c2]
              }
              
              # cat('\nDIAG1 i=',i,' c=',c,' n1=',n1,' n2=',n2,' c1=',c1,' c2=',c2)
              
              rep.row <- data.frame(Rig=temp$Rig[1],
                                    activity='Drilling',
                                    FromDatetime=fromDatetime,
                                    ToDatetime=toDatetime,
                                    FromDepth=temp$EDR_HoleDepth[fromDatetimeIndex],
                                    ToDepth=temp$EDR_HoleDepth[toDatetimeIndex],
                                    NumberOfConnections=c2-c1+1, 
                                    AvgWtoWConnectTime=sum(connectEvent$WtoWConnectTime[c1:c2]),   # AvgWtoWConnectTime=sum(temp$WtoWtimespan),
                                    AvgReamCCTime=sum(connectEvent$ReamCCTime[c1:c2]),             # AvgReamCCTime=sum(temp$ReamCCtimespan),
                                    AvgStoSConnectTime=sum(connectEvent$StoSConnectTime[c1:c2]),   # AvgStoSConnectTime=sum(rrs$timespan[rrs$values=='Connecting']),
                                    AvgSurveyTime=sum(connectEvent$SurveyTime[c1:c2]),             # AvgSurveyTime=sum(temp$Surveytimespan),
                                    StoSRFVote=sum(connectEvent$StoSRFVote[c1:c2]),                # StoSRFVote=sum(rrs$rfvote[rrs$values=='Connecting']),
                                    Missing=round(sum(connectEvent$Missing[c1:c2])/(temp$ID[n2]-temp$ID[n1]+1),digits=3),
                                    Drillers=if (prm.rc$singleDrillerReporting) temp$Driller[n1] else
                                      paste(as.character(unique(temp$Driller[n1:n2])),collapse=";"),
                                    Remark=paste(as.character(unique(temp$Remark[n1:n2])),collapse=";"),
                                    Warnings=if (sum(connectEvent$ReamCCTime[c1:c2])/(60*(c2-c1+1)) < prm.rc$ReamCCTime) 
                                      "" else sprintf("ReamCC Time >= %.1f minutes",prm.rc$ReamCCTime))
              rep.row$StoSRFVote <- rep.row$StoSRFVote/rep.row$AvgStoSConnectTime # AvgStoSConnectTime is sum weight at this point
              rep.row$AvgWtoWConnectTime <- rep.row$AvgWtoWConnectTime/(rep.row$NumberOfConnections*60)
              rep.row$AvgReamCCTime <- rep.row$AvgReamCCTime/(rep.row$NumberOfConnections*60)
              rep.row$AvgStoSConnectTime <- rep.row$AvgStoSConnectTime/(rep.row$NumberOfConnections*60)
              rep.row$AvgSurveyTime <- rep.row$AvgSurveyTime/(rep.row$NumberOfConnections*60)
              repconnect <- rbind(repconnect,rep.row)	
              c1 <- c+1
            }
          }
        }
      }
    } 
  }
  
  if (nrow(repconnect)>0) {
    repconnect$FromDepth <- round(repconnect$FromDepth)
    repconnect$ToDepth <- round(repconnect$ToDepth)
    repconnect$AvgWtoWConnectTime <- round(repconnect$AvgWtoWConnectTime,digits=1)
    repconnect$AvgReamCCTime <- round(repconnect$AvgReamCCTime,digits=1)
    repconnect$AvgStoSConnectTime <- round(repconnect$AvgStoSConnectTime,digits=1)
    repconnect$AvgSurveyTime <- round(repconnect$AvgSurveyTime,digits=1)
    # To clear up roundoff errors, make AvgWtoWConnectTime=AvgReamCCTime+AvgStoSConnectTime+AvgSurveyTime
    if (sum(abs(repconnect$AvgReamCCTime+repconnect$AvgStoSConnectTime+repconnect$AvgSurveyTime-repconnect$AvgWtoWConnectTime)>0.1)>0) {
      cat('\n\nperformancereport WARNING... found ',
          sum(abs(repconnect$AvgReamCCTime+repconnect$AvgStoSConnectTime+repconnect$AvgSurveyTime-repconnect$AvgWtoWConnectTime)>0.1),
          ' occurences of WtoW time not equal to ReamCC+StoS+Survey times... setting equal...\n')
    }
    repconnect$AvgWtoWConnectTime <- repconnect$AvgReamCCTime+repconnect$AvgStoSConnectTime+repconnect$AvgSurveyTime
    repconnect$StoSRFVote <- round(repconnect$StoSRFVote,digits=3)
  }
  if (minDeltaHoleDepthDropCount>0) {
    cat('\nConnection Time Report: Dropped ',minDeltaHoleDepthDropCount,
        ' "Connecting" events where delta hole depth following event was less than ',prm.rc$minDeltaHoleDepth,
        if (length(which(dt$BitDepth_UOM=='feet'))>0) 'feet.\n' else 'meters.\n')
  }
  if (minStoSConnectTimeDropCount>0) {
    cat('\nConnection Time Report: Dropped ',minStoSConnectTimeDropCount,
        ' "Connecting" events where StoS Connect time was less than ',prm.rc$minStoSConnectTime,' minutes.\n')
  }
  if (maxDeltaHoleDepthRateDropCount>0) {
    cat('\nConnection Time Report: Dropped ',maxDeltaHoleDepthRateDropCount,
        ' "Connecting" events where delta hole depth rate following event was greater than ',prm.t$maxDeltaHoledepthRate,
        if (length(which(dt$BitDepth_UOM=='feet'))>0) 'feet/sec.\n' else 'meters/sec.\n')
  }
  
  ######################################################################################
  # Create Tripping Speed Report
  # Aug 25 2016, add ActiveDeltaBitDepth column, subject ActiveTime to DeltaBitDepth limits

  reptspeed <- data.frame(Rig=NULL,activity=NULL, FromDatetime=NULL, ToDatetime=NULL, ActiveTime=NULL, 
                          FromBitDepth=NULL, ToBitDepth=NULL, ActiveDeltaBlockHeight=NULL,
                          ActiveDeltaBitDepth=NULL, ActiveDistanceTravelled=NULL,
                          NumberOfConnections=NULL, AvgSStoSConnectTime=NULL,BitInCasing=NULL,
                          RFVote=NULL,Missing=NULL,Drillers=NULL,Code=NULL,Remark=NULL,Warnings=NULL)
  
  # Nov 8, 2018, add tripping connect time detail report
  reptripconndetail <- data.frame(WellId=NULL,Rig=NULL,DrillerName=NULL,TripInOrOut=NULL,
                                  DateTimeStart=NULL,StoSConnectTime=NULL)

  ra <- data.frame(values=rle(as.vector(dt$activityDriller))$values,
                   lengths=rle(as.vector(dt$activityDriller))$lengths)
  ra$values <- as.character(ra$values)
  ra$values[substring(ra$values,1,8)=="Drilling"] <- "Drilling"
  ra$values[substring(ra$values,1,11)=="NotDrilling"] <- "NotDrilling"
  ra$values[substring(ra$values,1,10)=="TrippingIn"] <- "TrippingIn"
  ra$values[substring(ra$values,1,11)=="TrippingOut"] <- "TrippingOut"

  ra$from <- 0
  ra$to <- 0
  ra$to <- cumsum(ra$lengths)
  ra$from <- ra$to - ra$lengths + 1

  ra$fromDateTime <- NA
  ra$TripIn <- 0
  ra$TripOut <- 0
  ra$Connect <- 0
  ra$BitCasing <- NA
  ra$CrewBHA <- NA
  ra$BitDepth <- NA
  ra$HoleDepth <- NA
  ra$ROP <- NA
  ra$bhdratio <- NA
  ra$Missing <- NA
  
  for(i in 1:nrow(ra)) {
    ra$fromDateTime[i] <- dt$time[ra$from[i]]
    ra$TripIn[i] <- sum(dt[[prm.t$target]][ra$from[i]:ra$to[i]]=='TrippingIn' | dt[[prm.t$target]][ra$from[i]:ra$to[i]]=='TripInConnect')
    ra$TripOut[i] <- sum(dt[[prm.t$target]][ra$from[i]:ra$to[i]]=='TrippingOut' | dt[[prm.t$target]][ra$from[i]:ra$to[i]]=='TripOutConnect')
    ra$Connect[i] <- sum(dt[[prm.t$target]][ra$from[i]:ra$to[i]]=='Connecting')
    if (prm.rc$singleDrillerReporting==TRUE) { 
      if (length(unique(dt$IsBitInCasing[ra$from[i]:ra$to[i]])) != 1) cat('\nWARNING found non-unique casing status tripping reporting interval\n')
      ra$BitCasing[i] <- dt$IsBitInCasing[ra$from[i]]
      if (length(unique(dt$IsCrewHandlingBHA[ra$from[i]:ra$to[i]])) != 1) cat('\nWARNING found non-unique casing status tripping reporting interval\n')
      ra$CrewBHA[i] <- dt$IsCrewHandlingBHA[ra$from[i]]
    }
    ra$BitDepth[i] <- round(mean(dt$EDR_BitDepth[ra$from[i]:ra$to[i]],na.rm=TRUE),digits=2)
    ra$HoleDepth[i] <- round(mean(dt$EDR_HoleDepth[ra$from[i]:ra$to[i]],na.rm=TRUE),digits=2)
    ra$ROP[i] <- round(mean(dt$EDR_ROP[ra$from[i]:ra$to[i]],na.rm=TRUE),digits=3)
    ra$bhdratio[i] <- if(ra$HoleDepth[i]>0) round(ra$BitDepth[i]/ra$HoleDepth[i],digits=3) else 1
    # Determines proportion of missing values due to NAs that were omitted
    # First calculate the total number of values in the interval including the NAs
    ra$Missing[i] <- if (i==1) dt$ID[ra$to[i]] else (dt$ID[ra$to[i]] - dt$ID[ra$to[i-1]]) # catches NAs between the intervals that were omitted
    ra$Missing[i] <- if (ra$Missing[i]>0) round((ra$Missing[i]-(ra$to[i]-ra$from[i]+1))/ra$Missing[i],digits=3) else NA
  }
  cat('\nPerformance reporting activity (values Column)
    versus rig_state TrippingIn & TrippingOut\n\n')
  cat('\nTotal number of original rows=',dt$ID[nrow(dt)],' Number of complete rows=',nrow(dt))
  cat('\nTotal number of missing data (NA) rows =',dt$ID[nrow(dt)]-nrow(dt),' Proportion=',
      round((dt$ID[nrow(dt)]-nrow(dt))/dt$ID[nrow(dt)],digits=6),'\n')
  print(ra)
  
  tripConnectMinDeltaBitDepthRejectCount <- 0
  
  for (i in 1:nrow(ra)) {
    if (ra$values[i]=='TrippingOut') {
      temp <- dt[ra$from[i]:ra$to[i],]
      temp[[prm.t$target]] <- as.character(temp[[prm.t$target]])
      # preserve column with rig states 'TripOutConnect' and 'TripInConnect' for 
      # tripping 'NumberOfConnections' & 'AvgStoSConnectTime' report columns Oct 20, 2018
      temp$rig_state_trip_connect <- temp[[prm.t$target]]  
      # convert 'TripOutConnect' to 'TrippingOUt' rig states in temp for original section of this report, FHS 181018 
      temp[[prm.t$target]][temp[[prm.t$target]]=='TripOutConnect'] <- 'TrippingOut'
      warningMessage <- ''
      # computes fromDatetime and toDatetime including interpolation to adjacent observation, FHS 170418
      if (sum(temp[[prm.t$target]]=='TrippingOut')>0) {
        fromDatetimeIndex <- which(as.POSIXlt(temp$time,"%Y-%m-%d %H:%M:%S",tz="")==
                                     min(as.POSIXlt(temp$time[temp[[prm.t$target]]=='TrippingOut'],"%Y-%m-%d %H:%M:%S",tz="")))
        if (fromDatetimeIndex>1) {
          fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex]))+
                                                   as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex-1])))/2,
                                                   origin="1970-01-01 00:00:00"))
        } else {
          # At the beginning of the activity interval with fromDatetimeIndex=1, 
          # so look at time halfway to previous observation before current activity
          if(ra$from[i]>1) {
            fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$from[i]]))+
                                                     as.numeric(as.POSIXlt(dt$time[ra$from[i]-1])))/2,
                                                     origin="1970-01-01 00:00:00"))
          } else {
            # First observation in dt data.frame, so extrapolate half timespan
            fromDatetime <- as.character(as.POSIXlt(temp$time[fromDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")-
                                           temp$timespan[fromDatetimeIndex]/2)
          }
        }
        fromBitDepth <- temp$EDR_BitDepth[fromDatetimeIndex] - temp$deltaBitDepth[fromDatetimeIndex]
        toDatetimeIndex <- which(as.POSIXlt(temp$time,"%Y-%m-%d %H:%M:%S",tz="")==
                                   max(as.POSIXlt(temp$time[temp[[prm.t$target]]=='TrippingOut'],"%Y-%m-%d %H:%M:%S",tz="")))
        if (toDatetimeIndex<nrow(temp)) {
          toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[toDatetimeIndex]))+
                                                 as.numeric(as.POSIXlt(temp$time[toDatetimeIndex+1])))/2,
                                                 origin="1970-01-01 00:00:00"))
        } else {
          # At the end of the activity interval with toDatetimeIndex=nrow(temp), 
          # so look at time halfway to next observation after current activity
          if (ra$to[i]<nrow(dt)) {
            toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$to[i]]))+
                                                     as.numeric(as.POSIXlt(dt$time[ra$to[i]+1])))/2,
                                                  origin="1970-01-01 00:00:00"))
          } else {
            # Last observation in dt data.frame, so extrapolate half timespan
            toDatetime <- as.character(as.POSIXlt(temp$time[toDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")+
                                         temp$timespan[toDatetimeIndex]/2)
          }
        }
        toBitDepth <- temp$EDR_BitDepth[toDatetimeIndex]
      } else {
        fromDatetime <- NA
        toDatetime <- NA
        fromBitDepth <- NA
        toBitDepth <- NA
      }

      rep.row <- data.frame(Rig=temp$Rig[1],
                            activity='TrippingOut',
                            FromDatetime=fromDatetime,
                            ToDatetime=toDatetime,
                            ActiveTime=sum(temp$timespan[temp[[prm.t$target]]=='TrippingOut'])/3600,
                            FromBitDepth=fromBitDepth,
                            ToBitDepth=toBitDepth,
                            ActiveDeltaBlockHeight=0,
                            ActiveDeltaBitDepth=0,
                            ActiveDistanceTravelled=0,
                            NumberOfConnections=0,
                            AvgStoSConnectTime=0,
                            BitInCasing=ra$BitCasing[i],
                            CrewHandlingBHA=ra$CrewBHA[i],
                            RFVote=0,
                            Missing=ra$Missing[i],
                            Drillers=paste(as.character(unique(temp$Driller[temp[[prm.t$target]]=='TrippingOut'])),collapse=";"),
                            Code=paste(as.character(unique(temp$DrillingCode[temp[[prm.t$target]]=='TrippingOut'])),collapse=";"),
                            Remark=paste(as.character(unique(temp$Remark[temp[[prm.t$target]]=='TrippingOut'])),collapse=";"),
                            Warnings=warningMessage)
      if(rep.row$ActiveTime>0) {
        rep.row$RFVote <- sum(temp$timespan[temp[[prm.t$target]]=="TrippingOut"] * 
                                temp$rfvote[temp[[prm.t$target]]=="TrippingOut"])/
                          sum(temp$timespan[temp[[prm.t$target]]=="TrippingOut"])
        rep.row$ActiveDeltaBitDepth <- abs(sum(temp$deltaBitDepth[temp$deltaBitDepth<0 & temp[[prm.t$target]]=="TrippingOut"]))
        rep.row$ActiveDeltaBlockHeight <- abs(sum(temp$deltaBlockHeight[temp$deltaBlockHeight>0 & temp[[prm.t$target]]=="TrippingOut"]))
        rep.row$ActiveDistanceTravelled <- abs(sum(temp$deltaBitDepth[temp$deltaBitDepth<0 & 
                                                                      temp[[prm.t$target]]=="TrippingOut" &
                                                                      temp$EDR_BitDepth>prm.rc$bitDepthThreshold])) +   
                                           abs(sum(temp$deltaBlockHeight[temp$deltaBlockHeight>0 & 
                                                                         temp[[prm.t$target]]=="TrippingOut" &
                                                                         temp$EDR_BitDepth<=prm.rc$bitDepthThreshold]))
        # Does computations for tripping out connections, Oct 19, 2018
        ratc <- data.frame(values=rle(as.vector(temp$rig_state_trip_connect))$values,
                         lengths=rle(as.vector(temp$rig_state_trip_connect))$lengths)
        ratc$values <- as.character(ratc$values)
        ratc$from <- 0
        ratc$to <- 0
        ratc$to <- cumsum(ratc$lengths)
        ratc$from <- ratc$to - ratc$lengths + 1
        # compute timespan for each 'TripOutConnect' ratc record
        ratc$timespan <- 0 
        toConnectIndex <- which(ratc$values=='TripOutConnect')
        if (length(toConnectIndex)>0) {
          j <- toConnectIndex[1]
          ratc$timespan[j] <- sum(temp$timespan[ratc$from[j]:ratc$to[j]])
          # create detailed trip time report record, FHS Nov 8, 2018
          reptripconndetail <- rbind(reptripconndetail,data.frame(WellId=if(is.null(temp$WellName)) 'NONE' else temp$WellName[ratc$from[j]],
                                                                  Rig=temp$Rig[ratc$from[j]],
                                                                  DrillerName=if (prm.rc$singleDrillerReporting) temp$Driller[ratc$from[j]] else 
                                                                    paste(as.character(unique(temp$Driller[ratc$from[j]:ratc$to[j]])),collapse=";"),
                                                                  TripInOrOut='TrippingOut',
                                                                  DateTimeStart=temp$time[ratc$from[j]],
                                                                  StoSConnectTime=round(ratc$timespan[j]/60,digits=1)))
          if (length(toConnectIndex)>1) {
            for (k in 2:length(toConnectIndex)) {
              # compute delta bit depth 
              j2 <- toConnectIndex[k]
              j1 <- toConnectIndex[k-1]
              deltaBitDepth <- temp$EDR_BitDepth[ratc$from[j1]] - temp$EDR_BitDepth[ratc$from[j2]]
              if (deltaBitDepth > prm.rc$minDeltaHoleDepth) {
                ratc$timespan[j2] <- sum(temp$timespan[ratc$from[j2]:ratc$to[j2]])
                # create detailed trip time report record, FHS Nov 8, 2018
                reptripconndetail <- rbind(reptripconndetail,data.frame(WellId=if(is.null(temp$WellName)) 'NONE' else temp$WellName[ratc$from[j2]],
                                                                        Rig=temp$Rig[ratc$from[j2]],
                                                                        DrillerName=if (prm.rc$singleDrillerReporting) temp$Driller[ratc$from[j2]] else 
                                                                          paste(as.character(unique(temp$Driller[ratc$from[j2]:ratc$to[j2]])),collapse=";"),
                                                                        TripInOrOut='TrippingOut',
                                                                        DateTimeStart=temp$time[ratc$from[j2]],
                                                                        StoSConnectTime=round(ratc$timespan[j2]/60,digits=1)))
              } else {
                ratc$values[j2] <- 'Other'
                tripConnectMinDeltaBitDepthRejectCount <- tripConnectMinDeltaBitDepthRejectCount + 1
              }
            }
          }
          rep.row$NumberOfConnections <- sum(ratc$values == 'TripOutConnect')
          if (rep.row$NumberOfConnections>0) rep.row$AvgStoSConnectTime <- round((sum(ratc$timespan)/60)/rep.row$NumberOfConnections,digits=1)
        }

        if (rep.row$ActiveTime>=prm.rc$minActiveTime) reptspeed <- rbind(reptspeed,rep.row)	
      } else {
#         cat('\n\nWARNING!! Tripping Speed Report, found TrippingOut with active time equal zero\n')
#         cat('i=',i,' ra$values[i]=',ra$values[i],' ra$from[i]=',ra$from[i],' ra$to[i]=',ra$to[i],'\n')
      }
    } else if (ra$values[i]=='TrippingIn') {
      temp <- dt[ra$from[i]:ra$to[i],]
      temp[[prm.t$target]] <- as.character(temp[[prm.t$target]])
      # preserve column with rig states 'TripOutConnect' and 'TripInConnect' for 
      # tripping 'NumberOfConnections' & 'AvgStoSConnectTime' report columns Oct 20, 2018
      temp$rig_state_trip_connect <- temp[[prm.t$target]] 
      # convert 'TripOutConnect' to 'TrippingOUt' rig states in temp for original section of this report, FHS 181018  
      temp[[prm.t$target]][temp[[prm.t$target]]=='TripInConnect'] <- 'TrippingIn'
      warningMessage <- ''
      # computes FromDatetime and toDatetime including interpolation to adjacent observation, FHS 170418
      if (sum(temp[[prm.t$target]]=='TrippingIn')>0) {
        fromDatetimeIndex <- which(as.POSIXlt(temp$time,"%Y-%m-%d %H:%M:%S",tz="")==
                                     min(as.POSIXlt(temp$time[temp[[prm.t$target]]=='TrippingIn'],"%Y-%m-%d %H:%M:%S",tz="")))
        if (fromDatetimeIndex>1) {
          fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex]))+
                                                   as.numeric(as.POSIXlt(temp$time[fromDatetimeIndex-1])))/2,
                                                   origin="1970-01-01 00:00:00"))
        } else {
          # At the beginning of the activity interval with fromDatetimeIndex=1, 
          # so look at time halfway to previous observation before current activity
          if(ra$from[i]>1) {
            fromDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$from[i]]))+
                                                     as.numeric(as.POSIXlt(dt$time[ra$from[i]-1])))/2,
                                                     origin="1970-01-01 00:00:00"))
          } else {
            # First observation in dt data.frame, so extrapolate half timespan
            fromDatetime <- as.character(as.POSIXlt(temp$time[fromDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")-
                                           temp$timespan[fromDatetimeIndex]/2)
          }
        }
        fromBitDepth <- temp$EDR_BitDepth[fromDatetimeIndex] - temp$deltaBitDepth[fromDatetimeIndex]
        toDatetimeIndex <- which(as.POSIXlt(temp$time,"%Y-%m-%d %H:%M:%S",tz="")==
                                   max(as.POSIXlt(temp$time[temp[[prm.t$target]]=='TrippingIn'],"%Y-%m-%d %H:%M:%S",tz="")))
        if (toDatetimeIndex<nrow(temp)) {
          toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(temp$time[toDatetimeIndex]))+
                                                 as.numeric(as.POSIXlt(temp$time[toDatetimeIndex+1])))/2,
                                                 origin="1970-01-01 00:00:00"))
        } else {
          # At the end of the activity interval with toDatetimeIndex=nrow(temp), 
          # so look at time halfway to next observation beyond current activity
          if (ra$to[i]<nrow(dt)) {
            toDatetime <- as.character(as.POSIXlt((as.numeric(as.POSIXlt(dt$time[ra$to[i]]))+
                                                   as.numeric(as.POSIXlt(dt$time[ra$to[i]+1])))/2,
                                                   origin="1970-01-01 00:00:00"))
          } else {
            # Last observation in dt data.frame, extrapolate half timespan
            toDatetime <- as.character(as.POSIXlt(temp$time[toDatetimeIndex],"%Y-%m-%d %H:%M:%S",tz="")+
                                         temp$timespan[toDatetimeIndex]/2)
          }
        }
        toBitDepth <- temp$EDR_BitDepth[toDatetimeIndex]
      } else {
        fromDatetime <- NA
        toDatetime <- NA
        fromBitDepth <- NA
        toBitDepth <- NA
      }
      rep.row <- data.frame(Rig=temp$Rig[1],
                            activity='TrippingIn',
                            FromDatetime=fromDatetime,
                            ToDatetime=toDatetime,
                            ActiveTime=sum(temp$timespan[temp[[prm.t$target]]=='TrippingIn'])/3600,
                            FromBitDepth=fromBitDepth,
                            ToBitDepth=toBitDepth,
                            ActiveDeltaBlockHeight=0,
                            ActiveDeltaBitDepth=0,
                            ActiveDistanceTravelled=0,
                            NumberOfConnections=0,
                            AvgStoSConnectTime=0,
                            BitInCasing=ra$BitCasing[i],
                            CrewHandlingBHA=ra$CrewBHA[i],
                            RFVote=0,
                            Missing=ra$Missing[i],
                            Drillers=paste(as.character(unique(temp$Driller[temp[[prm.t$target]]=='TrippingIn'])),collapse=";"),
                            Code=paste(as.character(unique(temp$DrillingCode[temp[[prm.t$target]]=='TrippingIn'])),collapse=";"),
                            Remark=paste(as.character(unique(temp$Remark[temp[[prm.t$target]]=='TrippingIn'])),collapse=";"),
                            Warnings=warningMessage)
      if(rep.row$ActiveTime>0) {
        rep.row$RFVote <- sum(temp$timespan[temp[[prm.t$target]]=="TrippingIn"] * 
                                temp$rfvote[temp[[prm.t$target]]=="TrippingIn"])/
                          sum(temp$timespan[temp[[prm.t$target]]=="TrippingIn"])
        rep.row$ActiveDeltaBitDepth <- abs(sum(temp$deltaBitDepth[temp$deltaBitDepth>0 & temp[[prm.t$target]]=="TrippingIn"]))
        rep.row$ActiveDeltaBlockHeight <- abs(sum(temp$deltaBlockHeight[temp$deltaBlockHeight<0 & temp[[prm.t$target]]=="TrippingIn"]))
        rep.row$ActiveDistanceTravelled <- abs(sum(temp$deltaBitDepth[temp$deltaBitDepth>0 & 
                                                                        temp[[prm.t$target]]=="TrippingIn" &
                                                                        temp$EDR_BitDepth>prm.rc$bitDepthThreshold])) +   
                                           abs(sum(temp$deltaBlockHeight[temp$deltaBlockHeight<0 & 
                                                                         temp[[prm.t$target]]=="TrippingIn" &
                                                                         temp$EDR_BitDepth<=prm.rc$bitDepthThreshold]))
        # Does computations for tripping in connections, Oct 19, 2018
        ratc <- data.frame(values=rle(as.vector(temp$rig_state_trip_connect))$values,
                           lengths=rle(as.vector(temp$rig_state_trip_connect))$lengths)
        ratc$values <- as.character(ratc$values)
        ratc$from <- 0
        ratc$to <- 0
        ratc$to <- cumsum(ratc$lengths)
        ratc$from <- ratc$to - ratc$lengths + 1
        # compute timespan for each 'TripInConnect' ratc record
        ratc$timespan <- 0 
        toConnectIndex <- which(ratc$values=='TripInConnect')
        if (length(toConnectIndex)>0) {
          j <- toConnectIndex[1]
          ratc$timespan[j] <- sum(temp$timespan[ratc$from[j]:ratc$to[j]])
          # create detailed trip time report record, FHS Nov 8, 2018
          reptripconndetail <- rbind(reptripconndetail,data.frame(WellId=if(is.null(temp$WellName)) 'NONE' else temp$WellName[ratc$from[j]],
                                                                  Rig=temp$Rig[ratc$from[j]],
                                                                  DrillerName=if (prm.rc$singleDrillerReporting) temp$Driller[ratc$from[j]] else 
                                                                    paste(as.character(unique(temp$Driller[ratc$from[j]:ratc$to[j]])),collapse=";"),
                                                                  TripInOrOut='TrippingIn',
                                                                  DateTimeStart=temp$time[ratc$from[j]],
                                                                  StoSConnectTime=round(ratc$timespan[j]/60,digits=1)))
          if (length(toConnectIndex)>1) {
            for (k in 2:length(toConnectIndex)) {
              # compute delta bit depth 
              j2 <- toConnectIndex[k]
              j1 <- toConnectIndex[k-1]
              deltaBitDepth <- temp$EDR_BitDepth[ratc$from[j2]] - temp$EDR_BitDepth[ratc$from[j1]]
              if (deltaBitDepth > prm.rc$minDeltaHoleDepth) {
                ratc$timespan[j2] <- sum(temp$timespan[ratc$from[j2]:ratc$to[j2]])
                # create detailed trip time report record, FHS Nov 8, 2018
                reptripconndetail <- rbind(reptripconndetail,data.frame(WellId=if(is.null(temp$WellName)) 'NONE' else temp$WellName[ratc$from[j2]],
                                                                        Rig=temp$Rig[ratc$from[j2]],
                                                                        DrillerName=if (prm.rc$singleDrillerReporting) temp$Driller[ratc$from[j2]] else 
                                                                          paste(as.character(unique(temp$Driller[ratc$from[j2]:ratc$to[j2]])),collapse=";"),
                                                                        TripInOrOut='TrippingIn',
                                                                        DateTimeStart=temp$time[ratc$from[j2]],
                                                                        StoSConnectTime=round(ratc$timespan[j2]/60,digits=1)))
              } else {
                ratc$values[j2] <- 'Other'
                tripConnectMinDeltaBitDepthRejectCount <- tripConnectMinDeltaBitDepthRejectCount + 1
              }
            }
          }
        }
        rep.row$NumberOfConnections <- sum(ratc$values == 'TripInConnect')
        if (rep.row$NumberOfConnections>0) rep.row$AvgStoSConnectTime <- round((sum(ratc$timespan)/60)/rep.row$NumberOfConnections,digits=1)
                
        if (rep.row$ActiveTime>=prm.rc$minActiveTime) reptspeed <- rbind(reptspeed,rep.row)	
      } else {
#         cat('\n\nWARNING!! Tripping Speed Report, found TrippingIn with active time equal zero\n')
#         cat('i=',i,' ra$values[i]=',ra$values[i],' ra$from[i]=',ra$from[i],' ra$to[i]=',ra$to[i],'\n')
      }
    }
  }
  
  if (nrow(reptspeed)>0) {
    reptspeed$RFVote <- round(reptspeed$RFVote,digits=3)
  }
  
  if(prm.rc$verbose & tripConnectMinDeltaBitDepthRejectCount>0) {
    cat('\nRejected ',tripConnectMinDeltaBitDepthRejectCount,' tripping connect intervals due to delta bit depth less than ',
        prm.rc$minDeltaHoleDepth,' meters.\n')
  }

  # cat('\nperformancereports DIAG finished .. about to return\n')
  
  return(list(repconnect=repconnect,reptspeed=reptspeed,repconnectdetail=repconnectdetail,reptripconndetail=reptripconndetail))
}