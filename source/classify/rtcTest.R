#######################################################################################
# rtcTest.R Real Time Rig Classify Test V3.0 - test for new record
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 22, 2019
#######################################################################################
rtcTest <- function(process.start.time,rtc) {
  
  # Output to log file if requested
  if (nchar(rtc$prm.rt$outputLogFilename)>0) {
    sink(file=rtc$prm.rt$outputLogFilename, append=TRUE)
    options(width=132)
  }
  
  # Temporary - print R version for diagnostic testing, FHS Nov 25, 2017
  # print(sessionInfo())
  
  # Creates the SQL query
  startEpoch <- round(as.numeric(process.start.time) - rtc$prm.rt$lagDelay -
                        (rtc$prm.t$beforeCount+1+rtc$prm.t$afterCount)*rtc$prm.dc$timeInterval)*1000
  endEpoch <- round(as.numeric(process.start.time))*1000
  sql_query <- paste0("CALL `get_drilling_data_between_two_dates`(",
                      as.character(startEpoch),",",as.character(endEpoch),")")
  if (rtc$prm.rt$verbose) cat('\nsql_query=',sql_query)

  # Load the latest rig state classification data with SQL query
  sqlquery.start.time <- Sys.time()
  # sqlQuery call puts raw results into de
  de <- sqlQuery(rtc$ch,sql_query)
  if (rtc$prm.rt$verbose) cat('\nSQL query execution elapsed time = ',
        round(as.numeric(difftime(Sys.time(),sqlquery.start.time,units='secs')),digits=2),' secs.\n')
  rtc$sql$ntry <- rtc$sql$ntry+1
  if (class(de)!='data.frame') {
    if (rtc$prm.rt$verbose) {
      cat('\nProblems with sql query\n')
      print(de)
    }
    # rtc$errorCount = rtc$errorCount + 1
    rtc$sql$readErr <- rtc$sql$readErr + 1
  } else { # sql query returned a data.frame
    de$tenseconds_epoch <- as.POSIXlt(de$tenseconds_epoch/1000,origin="1970-01-01 00:00:00")
    # Ignore any records that are within rtc$prm.rt$lagDelay of process.start.time
    rtc$rec$lagdel <- sum(as.numeric(difftime(process.start.time,de$tenseconds_epoch,units='secs'))<rtc$prm.rt$lagDelay)
    if (rtc$prm.rt$verbose) cat('\nIgnoring ',rtc$rec$lagdel,' sql tag records within rtc$prm.rt$lagDelay=',
                                rtc$prm.rt$lagDelay,' of actual time=',as.character(process.start.time))
    de <- de[as.numeric(difftime(process.start.time,de$tenseconds_epoch,units='secs'))>=rtc$prm.rt$lagDelay,]
    rtc$rec$nrow <- nrow(de)
    rtc$rec$ncol <- ncol(de)
    
    # Consolidate two mudpump values into single total mudpump value
    if (!is.null(de$historian_mp1_spm) & !is.null(de$historian_mp2_spm)) {
      # historian_mp1_spm label
      de$historian_mp1_spm <- de$historian_mp1_spm + de$historian_mp2_spm
      de$historian_mp2_spm <- NULL
    } else if (!is.null(de$mp1_spm) & !is.null(de$mp2_spm)) {
      # mp1_spm label
      de$mp1_spm <- de$mp1_spm + de$mp2_spm
      de$mp2_spm <- NULL
    } 
    
    dtbIndex <- nrow(de) - rtc$prm.t$afterCount
    if (dtbIndex<1) dtbIndex <- 1
    current.observation.time <- de$tenseconds_epoch[dtbIndex]
    # If new current.observation time, then proceed with processing
    if (current.observation.time<=rtc$previous.observation.time) {
      rtc$sql$nskip <- rtc$sql$nskip + 1
      if (rtc$prm.rt$verbose) {
        cat('\nSkipping processing of this SQL query because its "current" time interval=', as.character(current.observation.time),
            ' is not ahead of the previously saved time interval=',as.character(rtc$previous.observation.time),'\n')
      }
    } else {
      if (rtc$prm.rt$verbose | (is.null(rtc$declean.previous) & rtc$elapsedTime < 0.01)) {
        cat('\nSQL Query time=',as.character(de$tenseconds_epoch[dtbIndex]),
            ' with ',nrow(de),' rows and ',ncol(de),' columns.\n')
        cat('\nList of first, "current" and last raw values retrieved from SQL query.\n')
        temp <- de[c(1,dtbIndex,nrow(de)),]
        for (c in colnames(temp)){
           temp[,c] <- as.character(temp[,c])
        }
        print(t(as.matrix(temp)))
      }
      
      # If option selected, performs dclean 
      if (nchar(rtc$prm.rt$dcleanPrmFilename)>0) {
        if (rtc$prm.dc$reshapeHistorianData==TRUE) {
          de <- reshape(de,rtc$prm.dc)
        }
        # performs dclean without interpolating over any missing values with rtc$prm.dc.zeroMaxLook
        declean.current <- dclean(de,rtc$prm.dc.zeroMaxLook)
      } else {
        declean.current <- de
      }
      
      # Determine raw NA count
      select.cols <- rtc$prm.t$cp$name[rtc$prm.t$cp$name %in% colnames(declean.current)]
      if (length(select.cols) < length(rtc$prm.t$cp$name)) {
        rtc$rec$nNAraw <- rtc$rec$nNAraw + (length(rtc$prm.t$cp$name)-length(select.cols)) * rtc$rec$nrow
      }
      rtc$rec$nNAraw <- rtc$rec$nNAraw + sum(is.na(declean.current[,select.cols]))
      
      # Determine raw NA count for each predictor
      tempPred.NAraw <- rep(0,length(rtc$prm.t$cp$name)) # temporary variable to be accumulated if successful completion
      for (i in 1:length(rtc$prm.t$cp$name)) {
        c <- as.character(rtc$prm.t$cp$name[i])
        if (c %in% colnames(declean.current)) tempPred.NAraw[i] <- sum(is.na(declean.current[,c]))
      }
      
      # If option selected, performs dclean 
      # This time interpolates any missing values with rtc$prm.dc
      if (nchar(rtc$prm.rt$dcleanPrmFilename)>0) {
        # if first time around, then turns on verbose for dclean
        tempVerboseStatus <- rtc$prm.dc$verbose; if (is.null(rtc$declean.previous) & rtc$elapsedTime < 0.01) rtc$prm.dc$verbose <- TRUE
        declean.current <- dclean(de,rtc$prm.dc) 
        rtc$prm.dc$verbose <- tempVerboseStatus # restore orginal verbose status
      }
      # If missing EDR_TorqueValue column option selected and column is missing then
      # creates column with numeric value used in model, FHS 3/8/2019
      if (rtc$prm.t$missingTorque) {
        if (is.null(declean.current$EDR_RotaryTorque)) {
          declean.current$EDR_RotaryTorque <- rtc$prm.t$missingTorqueValue
          if (rtc$prm.rt$verbose) cat('\n\nEDR_RotaryTorque column missing and option selected to create column with value=',
              rtc$prm.t$missingTorqueValue,'\n\n')
        } else {
          if (sum(is.na(declean.current$EDR_RotaryTorque))>0) {
            if (rtc$prm.rt$verbose) cat('\n\nEDR_RotaryTorque column has ',sum(is.na(declean.current$EDR_RotaryTorque)),
                ' missing values, option selected to replace them with value=',rtc$prm.t$missingTorqueValue,'\n\n')
            declean.current$EDR_RotaryTorque[is.na(declean.current$EDR_RotaryTorque)] <- rtc$prm.t$missingTorqueValue
          }
        }
      }
      
      # Determine clean NA count
      select.cols <- rtc$prm.t$cp$name[rtc$prm.t$cp$name %in% colnames(declean.current)]
      if (length(select.cols) < length(rtc$prm.t$cp$name)) {
        rtc$rec$nNAclean <- rtc$rec$nNAclean + (length(rtc$prm.t$cp$name)-length(select.cols)) * rtc$rec$nrow
      }
      rtc$rec$nNAclean <- rtc$rec$nNAclean + sum(is.na(declean.current[,select.cols]))

      # Check for mismatch between current and previous
      if (!is.null(rtc$declean.previous)) {
        # determine the matching time stamp current and previous record indices 
        select.current <- which(declean.current$time %in% rtc$declean.previous$time)
        select.previous <- which(rtc$declean.previous$time %in% declean.current$time)
        rtc$rec$noverlap <- length(select.current)

        select.cols <- rtc$prm.t$cp$name[rtc$prm.t$cp$name %in% colnames(declean.current) &
                                         rtc$prm.t$cp$name %in% colnames(rtc$declean.previous)]

        # Sums all of the current versus previous missmatches
        # (different numeric value) plus (current NA and previous non-NA) plus (current non-NA and previous NA)
        rtc$rec$missMatch <- sum(abs(declean.current[select.current,select.cols] - rtc$declean.previous[select.previous,select.cols])>rtc$prm.rt$matchTolerance, na.rm=T) +
          sum(is.na(declean.current[select.current,select.cols]) & !is.na(rtc$declean.previous[select.previous,select.cols])) +
          sum(!is.na(declean.current[select.current,select.cols]) & is.na(rtc$declean.previous[select.previous,select.cols]))

        # Sums the current versus previous missmatches by predictor column 
        tempPred.missMatch <- rep(0,length(rtc$prm.t$cp$name)) # temporary variable to be accumulated if successful completion
        for (i in 1:length(rtc$prm.t$cp$name)) {
          c <- as.character(rtc$prm.t$cp$name[i])
          if (c %in% colnames(declean.current) & c %in% colnames(rtc$declean.previous)) tempPred.missMatch[i] <- 
              sum(abs(declean.current[select.current,c] - rtc$declean.previous[select.previous,c])>rtc$prm.rt$matchTolerance, na.rm=T) +
              sum(is.na(declean.current[select.current,c]) & !is.na(rtc$declean.previous[select.previous,c])) +
              sum(!is.na(declean.current[select.current,c]) & is.na(rtc$declean.previous[select.previous,c]))
        }
        
        # check for case where current individual missing values are available in previous
        if (sum(is.na(declean.current[select.current,select.cols]) & !is.na(rtc$declean.previous[select.previous,select.cols]))>0) {
          selected.missing <- is.na(declean.current[select.current,select.cols]) & !is.na(rtc$declean.previous[select.previous,select.cols])
          if (rtc$prm.rt$verbose) cat('\nThere are ',sum(selected.missing),' current missing values that are being retrieved from previous SQL query.')
          declean.current[select.current,select.cols][selected.missing] <- rtc$declean.previous[select.previous,select.cols][selected.missing]
        }
        
        # Check for case where current columns are missing, but are available in previous
        select.cols <- rtc$prm.t$cp$name[!rtc$prm.t$cp$name %in% colnames(declean.current) & rtc$prm.t$cp$name %in% colnames(rtc$declean.previous)]
        if (length(select.cols>0)) {
          if (rtc$prm.rt$verbose) cat('\nMissing column(s) ',select.cols,' retrieving values from previous SQL query')
          rtc$rec$missMatch <- rtc$rec$missMatch + length(select.cols) * length(select.current)
          itemp <- rtc$prm.t$cp$name %in% select.cols
          tempPred.missMatch[itemp] <- tempPred.missMatch[itemp] + length(select.current)
          declean.current[,select.cols] <- NA  # default missing columns filled with missing values
          declean.current[select.current,select.cols] <- rtc$declean.previous[select.previous,select.cols]
        }
      }

      de <- declean.current

      if (rtc$prm.rt$verbose) cat('\nBuilding predictor feature variables.\n')
      # if first time around, then turns on verbose for dbuild
      tempVerboseStatus <- rtc$prm.t$verbose; if (is.null(rtc$declean.previous) & rtc$elapsedTime < 0.01) rtc$prm.t$verbose <- TRUE
      dtb <- tryCatch({
        dbuild(de,prm.t=rtc$prm.t,rtc$eigenRes)
      },
      error=function(e) {
        cat('\n\nERRORS in dbuild, UNABLE TO PERFORM RIG STATE CLASSIFICATION:\n',e$message,'\n\n')
        rtc$sql$incomplete <<- rtc$sql$incomplete + 1  # <<- to set incomplete flag outside of function (globally)
        return(NULL) # returns NULL to dtb variable
      })
      if (rtc$prm.t$verbose) cat('\n\n')
      rtc$prm.t$verbose <- tempVerboseStatus # restore original verbose status
      
      if (!is.null(dtb)) { # Only continue processing if dbuild was successful
        # Sets up ID to keep track for dropped records due to incomplete cases
        dtb$ID <- 1:nrow(dtb)
        # computes index for dtb record to be saved in this round
        dtbIndex <- nrow(dtb) - rtc$prm.t$afterCount
        if (dtbIndex>0) {
          # Makes sure that the record to be saved in this round (dtbIndex) is a complete case
          # if (nrow(datatest)==0) {
          if (!dtb$ID[dtbIndex] %in% dtb[complete.cases(dtb[rtc$prm.t$tpnames]),c(rtc$prm.t$tpnames,'ID')]$ID) {
            if (rtc$prm.rt$verbose) {
              cat('\n\nRecord from ',as.character(dtb$time[dtbIndex]),' is incomplete and will not be saved')
              cat('\nmissing values=',names(dtb[dtbIndex,rtc$prm.t$tpnames])[which(is.na(dtb[dtbIndex,rtc$prm.t$tpnames]))])
            }
            dtb <- NULL
            rtc$sql$incomplete <- rtc$sql$incomplete + 1
          }
        } else {
          cat('\nInsufficient sql query records=',nrow(dtb),' for rtc$prm.t$afterCount=',
              rtc$prm.t$afterCount,' lag time offset, skipping....')
          dtb <- NULL
          rtc$sql$incomplete <- rtc$sql$incomplete + 1
        }
      }  
      
      if (!is.null(dtb)) { # Only continue processing if single rig state available
        # Sets up ID to keep track for dropped records due to incomplete cases
        dtb$ID <- 1:nrow(dtb)
        # Sets up complete cases for calculating random forest results
        ID.test <- dtb[complete.cases(dtb[rtc$prm.t$tpnames]),'ID']
        datatest <- dtb[complete.cases(dtb[rtc$prm.t$tpnames]),rtc$prm.t$tpnames]
        # Produce up random forest results from model
        rf.res <- data.frame(ID=ID.test)
        rf.res[[rtc$prm.t$target]] <- predict(rtc$rf,datatest)
        
        # Look at random forest votes
        rf.votes <- as.data.frame(predict(rtc$rf,datatest,type="vote"))
        rf.res$rfvote <- 0
        for (c in colnames(rf.votes)) {
          rf.res$rfvote[rf.res[[rtc$prm.t$target]]==c] <- rf.votes[[c]][rf.res[[rtc$prm.t$target]]==c]
        }
        
        if (rtc$prm.rt$verbose) {
          cat('\nRandom Forest Decision Tree Single Classification Proportional Votes\n')
          df <- data.frame(names=as.data.frame(table(rf.res[[rtc$prm.t$target]]))[,1])
          df <- merge(df,aggregate(.~names,data=cbind(names=rf.res[[rtc$prm.t$target]],as.data.frame(rf.votes)),
                                   FUN=function(x){mean(x)}),by='names',all.x=TRUE)
          rownames(df) <- df$names
          df <- as.matrix(df[,2:ncol(df)])
          df <- cbind(df,vote.confidence=rowSums(df * diag(ncol(df))))
          df <- cbind(df,count=as.vector(table(rf.res[[rtc$prm.t$target]])))
          df[,1:(ncol(df)-1)] <- round(df[,1:(ncol(df)-1)],digits=3)
          df <- as.data.frame(df)
          df <- df[df$count>0,]
          print(df)
        }
        
        dt <- merge(dtb,rf.res,by="ID",all.x=TRUE)
        
        # Assigns values to rig state and rfvote NA values
        dt$rfvote[is.na(dt$rfvote)] <- 0 # Sets any rfvote NAs to zero
        dt[[rtc$prm.t$target]] <- as.character(dt[[rtc$prm.t$target]])
        dt[[rtc$prm.t$target]][is.na(dt[[rtc$prm.t$target]])] <- 'Data_Incomplete'
        dt[[rtc$prm.t$target]] <- as.factor(dt[[rtc$prm.t$target]])
        
        if (rtc$prm.rt$verbose) {
          cat('\nThe rig state classification quantities are as follows:')
          if (sum(is.na(dt[[rtc$prm.t$target]]))>0) cat('\nThere are ',sum(is.na(dt[[rtc$prm.t$target]])),' observations without rig state classifications (NA values).')
          print(table(dt[[rtc$prm.t$target]]))
          cat('\nOriginal data nrow(de)=',nrow(de),' rig classification data nrow(dt)=',nrow(dt),
              ' dtbIndex=',dtbIndex,' rtc$prm.t$afterCount=',rtc$prm.t$afterCount,'\n')
          cat('\nMerging rig state classifications with original data.\n')
        }
        de$time <- as.character(de$time)
        dt$time <- as.character(dt$time)
        if (nrow(de) != length(unique(de$time))) {
          cat('WARNING!!! there are ',nrow(de)-length(unique(de$time)),
              ' non-unique time values in original data.\n')
        }
        if (nrow(dt) != length(unique(dt$time))) {
          cat('WARNING!!! there are ',nrow(dt)-length(unique(dt$time)),
              ' non-unique time values in rig state classification data.\n')
        }
        
        de <- merge(de,dt[,c('time',rtc$prm.t$target,'rfvote')],by='time')
        if (rtc$prm.t$timeColName != 'time') de <- de[,!(colnames(de) %in% 'time')] # drop the extra 'time' column
        
        # Post processing option rig state consolidation
        if (rtc$prm.rt$ppflag==TRUE) {
          if (rtc$prm.rt$verbose) cat('\nPerforming post processing consolidation of classifications\n')
          dt$Rig <- rtc$prm.rt$ODBCConnectName # a Rig name is needed for dreport
          rep <- dreport(dt,rtc$prm.t,rtc$prm.t$target)
          # If there were incomplete observations for rf, must fill in blanks with rf.votes
          if (nrow(rf.votes)<nrow(dt)) {
            rf.votes <- cbind(rf.res$ID,rf.votes)
            colnames(rf.votes) <- c('ID',colnames(rf.votes)[2:ncol(rf.votes)])
            rf.votes <- merge(data.frame(ID=dtb$ID),rf.votes,by="ID",all.x=TRUE)
            rf.votes <- rf.votes[order(rf.votes$ID),] # sort by ascending ID number
            rf.votes[is.na(rf.votes)] <- 0 # missing rfvote values are set to zero
          }
          dt <- postprocess(dt,rep,rtc$prm.rt,rtc$prm.t,rf.votes)
        }
        
        # Determine if there are missing intervals between current and previous record
        if (nrow(rtc$dt.output)>0) {
          rtc$rec$missInt <- as.numeric(difftime(as.POSIXlt(dt$time[dtbIndex],"%Y-%m-%d %H:%M:%S",tz=""),
                                                 rtc$dt.output$time[nrow(rtc$dt.output)],units='sec')) / rtc$prm.dc$timeInterval - 1
        } else {
          rtc$rec$missInt <- 0
        }
        
        # create temp.dt.output for single record from this rtcTest call - FHS Dec 3, 2018
        # If there is an error creating this record then it doesn't corrupt the entire rtc$dt.output record
        temp.dt.output <- tryCatch({data.frame(time=as.POSIXlt(dt$time[dtbIndex],"%Y-%m-%d %H:%M:%S",tz=""),
                                               epoch=as.character(as.numeric(as.POSIXlt(dt$time[dtbIndex],"%Y-%m-%d %H:%M:%S",tz=""))*1000),
                                               rig_state=as.character(dt$rig_state[dtbIndex]),
                                               code=as.character(rtc$prm.rt$rigStateCode$code[rtc$prm.rt$rigStateCode$rig_state %in% dt$rig_state[dtbIndex]]),
                                               rfvote=dt$rfvote[dtbIndex],
                                               postProcess=as.character(rf.res$rig_state[dtbIndex])!=as.character(dt$rig_state[dtbIndex]),
                                               lagTime=round(as.numeric(difftime(Sys.time(),as.POSIXlt(dt$time[dtbIndex],"%Y-%m-%d %H:%M:%S",tz=""),units='secs')),digits=1),
                                               processTime=round(as.numeric(difftime(Sys.time(),process.start.time,units='secs')),digits=3),
                                               ntry=rtc$sql$ntry,
                                               readErr=rtc$sql$readErr,
                                               nskip=rtc$sql$nskip,
                                               incomplete=rtc$sql$incomplete,
                                               writeErr=rtc$sql$writeErr,
                                               lagdel=rtc$rec$lagdel,
                                               nrow=rtc$rec$nrow,
                                               ncol=rtc$rec$ncol,
                                               noverlap=rtc$rec$noverlap,
                                               nNAraw=rtc$rec$nNAraw,
                                               nNAclean=rtc$rec$nNAclean,
                                               missMatch=rtc$rec$missMatch,
                                               missInt=rtc$rec$missInt)
                                    },
                                  error=function(e) {
                                    cat('\n\nERRORS in rtcTest, UNABLE TO ASSIGN RECORD vALUES to temp.dt.output:\n',e$message,'\n\n')
                                    # Test for missing rig state numeric code
                                    tryCatch({
                                      if (sum(rtc$prm.rt$rigStateCode$rig_state %in% dt$rig_state[dtbIndex])==0) {
                                        cat('\nERROR LIKELY CAUSED BY NO NUMERIC CODE FOR rig state=',as.character(dt$rig_state[dtbIndex]))
                                      }
                                    })
                                    temp.dt.output <- NULL
                                  })
        if (!is.null(temp.dt.output)) {
          # append temp.dt.output to rtc$dt.output
          rtc$dt.output <- tryCatch({rbind(rtc$dt.output,temp.dt.output)},
                                    error=function(e) {
                                      cat('\n\nERRORS in rtcTest, UNABLE TO APPEND temp.dt.output record to rtc$dt.output:\n',e$message,'\n\n')
                                      temp.dt.output <- NULL
                                    })   
        }

        if (!is.null(temp.dt.output)) {
          # accumulate statistics by predictor
          for (i in 1:length(rtc$prm.t$cp$name)) {
            c <- rtc$prm.t$cp$name[i]
            select.current <- !is.na(de[,c])
            rtc$cumPredStats$n[i] <- rtc$cumPredStats$n[i] + sum(select.current)
            if (min(de[select.current,c])<rtc$cumPredStats$min[i]) rtc$cumPredStats$min[i] <- min(de[select.current,c])
            if (max(de[select.current,c])>rtc$cumPredStats$max[i]) rtc$cumPredStats$max[i] <- max(de[select.current,c])
            rtc$cumPredStats$NAclean[i] <- rtc$cumPredStats$NAclean[i] + sum(is.na(de[,c]))
            rtc$cumPredStats$s1[i] <- rtc$cumPredStats$s1[i] + sum(de[select.current,c])
            rtc$cumPredStats$s2[i] <- rtc$cumPredStats$s2[i] + sum(de[select.current,c]^2)
            # Seek UOM column and corresponding values if first round
            if (c %in% rtc$prm.t$standardUOM$dColName & is.null(rtc$declean.previous)) {
              uomColName <- rtc$prm.t$standardUOM$uomColName[rtc$prm.t$standardUOM$dColName %in% c][1]
              if (uomColName %in% colnames(de)) { # make sure UOM column exists
                rtc$cumPredStats$SQL_UOM <- as.character(rtc$cumPredStats$SQL_UOM)
                select.current.UOM <- !is.na(de[,uomColName])
                rtc$cumPredStats$SQL_UOM[i] <- unique(de[select.current.UOM,uomColName])[1] # pre-conversion UOM
                if ( unique(de[select.current.UOM,uomColName])[1] != unique(dtb[select.current.UOM,uomColName])[1]) {
                  # UOM was converted, fills in UOM (post conversion)
                  rtc$cumPredStats$UOM <- as.character(rtc$cumPredStats$UOM)
                  rtc$cumPredStats$UOM[i] <- unique(dtb[,uomColName])[1] # post-conversion UOM
                  # finds UOM conversion row with formula
                  UOM.conversion.row <- which(rtc$prm.t$standardUOM$dColName %in% c &
                                              rtc$prm.t$standardUOM$inputUnit %in% rtc$cumPredStats$SQL_UOM[i] &
                                              rtc$prm.t$standardUOM$outputUnit %in% rtc$cumPredStats$UOM[i])
                  if (length(UOM.conversion.row)>0) {
                    rtc$cumPredStats$formula <- as.character(rtc$cumPredStats$formula)
                    rtc$cumPredStats$formula[i] <- rtc$prm.t$standardUOM$formula[UOM.conversion.row[1]]
                  } else {
                    rtc$cumPredStats$formula <- as.character(rtc$cumPredStats$formula)
                    rtc$cumPredStats$formula[i] <- "Not Found"
                  }
                }
              }
            }
          }
          if (exists('tempPred.NAraw')) rtc$cumPredStats$NAraw <- rtc$cumPredStats$NAraw + tempPred.NAraw
          if (exists('tempPred.missMatch')) rtc$cumPredStats$missMatch <- rtc$cumPredStats$missMatch + tempPred.missMatch
          # success on current rig state classification, reset previous observation time for next try
          rtc$previous.observation.time <- current.observation.time
          rtc$declean.previous <- de
          
          # Write time epoch and rig_state code to database
          if (rtc$prm.rt$sqlWriteRigStateCode) {
            if (rtc$prm.rt$verbose) cat('\nWriting epoch and rig_state code to database with sql procedure.\n')
            feedback <- sqlQuery(rtc$ch,paste0("CALL `insert_derived_rig_state_record`('",as.character(rtc$dt.output$epoch[nrow(rtc$dt.output)]),
                                               "','",as.character(rtc$dt.output$code[nrow(rtc$dt.output)]),"')"))
            tryCatch({
              if (feedback[1,1] != 1) {
                if (rtc$prm.rt$verbose) {
                  cat('\nIrregular database write query feedback:\n')
                  print(feedback)
                }
                rtc$dt.output$writeErr[nrow(rtc$dt.output)] <<- 1  # <<- to set writeErr flag outside of function (globally)
              }
            },
            error=function(e) {
              if (rtc$prm.rt$verbose) {
                cat('\nIrregular database write query.  Feedback Error follows:\n')
                print(feedback)
                cat('ERRORS in database write query feedback:\n',e$message,'\n')
              }
              rtc$dt.output$writeErr[nrow(rtc$dt.output)] <<- 1 # <<- to set writeErr flag outside of function (globally)
            })
          }
          # Write results to log file
          write.table(rtc$dt.output[nrow(rtc$dt.output),],col.names=if(nrow(rtc$dt.output)==1 | rtc$prm.rt$verbose) T else F,row.names=F)
          if (rtc$prm.rt$verbose) cat(paste(rep('-',80),collapse=''),'\n\n')

          # Initialize sql statistics that are saved with each new record
          # rtc$sql. accumulated values through multiple calls to rtcTest, reset when record saved
          rtc$sql <- list(ntry=0,       # count of sql query trys
                          readErr=0,       # count of non-dataframe returns from sql
                          nskip=0,      # count of skipped trys with same time interval as previous record
                          incomplete=0, # incomplete principal records
                          writeErr=0)   # SQL write error
          # rtc$rec.  statistics on current sql query that is recorded
          rtc$rec <- list(lagdel=0,   # Number of records ignored within lag time delay of actual time
                          nrow=0,     # Number of rows in current query
                          ncol=0,     # Number of cols in current query
                          noverlap=0, # Number of overlapping records with previous query
                          # statistics on post dclean sql query with no interpolation
                          nNAraw=0,      # NA count for all predictors before dclean interpolation/extrapolation
                          nNAclean=0,    # NA count for all predictors after clean interpolation/extrapolation
                          missMatch=0,   # non-matching numeric values with previous query for all predictors
                          # statistics on post dclean sql query with interpolation
                          missInt=0)   # Number of missed intervals since last record
                          
          # Build diagnostics file if requested
          if (nchar(rtc$prm.rt$outputDiagFilename)>0) {
            dtIndex <- nrow(dt) - rtc$prm.t$afterCount
            if (dtIndex<1) dtIndex <- 1
            rtc$dt.diag <- rbind(rtc$dt.diag,dt[dtIndex,])
          }
        } # !is.null(temp.dt.output) writing result to database with SQL
      } # !is.null(dtb), rig state to be saved is available
    } # new observation i.e current.observation.time>rtc$previous.observation.time 
  } # SQL query return valid data.frame i.e. class(de)=='data.frame'
  
  if (sink.number()>0) sink(file=NULL)
  
  return(rtc)
}