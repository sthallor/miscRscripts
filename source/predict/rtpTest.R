#######################################################################################
# rtpTest.R - testing routine for real time predictions
# Ensign Energy Services Inc. retains all rights to this software
# FHS, June 5, 2017
#######################################################################################
rtpTest <- function(process.start.time,rtp) {
  
  if (nchar(rtp$prm.rp$outputLogFilename)>0) {
    sink(file=rtp$prm.rp$outputLogFilename,append=TRUE)
    options(width=132)
  }
  # Sets query starEpoch and endEpoch that includes prm.pr$lagDelay
  if (is.null(rtp$dt.all)) {
    startEpoch <- round(as.numeric(process.start.time) - rtp$prm.rp$lagDelay - 
                          rtp$prm.pt$rowPastSpan*rtp$prm.pt$dominantTimeInterval -
                          rtp$prm.rp$SQLqueryBufferTime)*1000
  } else {
    startEpoch <- round(as.numeric(process.start.time) - rtp$prm.rp$lagDelay -
                          rtp$prm.rp$SQLqueryBufferTime)*1000
  }
  endEpoch <- round(as.numeric(process.start.time))*1000
  
  sql_query <- paste0("CALL `get_igbt_data_between_two_dates`(",
                      as.character(startEpoch),",",as.character(endEpoch),")")
  if (rtp$prm.rp$verbose) cat('\nrtp$ch=',rtp$ch,' sql_query=',sql_query)
  
  tags <- sqlQuery(rtp$ch,sql_query)
  
  rtp$ntry <- rtp$ntry+1
  if (class(tags)=='data.frame') {
    if (rtp$prm.rp$verbose) cat('\nsql_query returned tags dataframe with ',
                                nrow(tags),' rows and ',ncol(tags),' cols.')
    # Converts epoch time stamp into POSIXlt
    tags[[rtp$prm.dc$timeColName]] <- as.POSIXlt(tags[[rtp$prm.dc$timeColName]]/1000,origin="1970-01-01 00:00:00")
    
    # Delete any records that are within rtp$prm.rp$lagDelay of process.start.time
    if (rtp$prm.rp$verbose) cat('\nDeleting ',sum(as.numeric(difftime(process.start.time,tags[[rtp$prm.dc$timeColName]],units='secs'))<rtp$prm.rp$lagDelay),
                                ' sql tag records within rtp$prm.rp$lagDelay=',rtp$prm.rp$lagDelay,' of current time=',as.character(process.start.time))
    tags <- tags[as.numeric(difftime(process.start.time,tags[[rtp$prm.dc$timeColName]],units='secs'))>=rtp$prm.rp$lagDelay,]
    
    # If option selected, performs dclean 
    if (nchar(rtp$prm.rp$dcleanPrmFilename)>0) {
      if (rtp$prm.dc$reshapeHistorianData==TRUE) {
        dt.sql <- reshape(tags,rtp$prm.dc)
      } else {
        dt.sql <- tags
      }
      dt.sql <- dclean(dt.sql,rtp$prm.dc)
    }
    # current observations time must have all data present (complete.cases)
    current.observation.time <- max(dt.sql$time[complete.cases(dt.sql)])
    if (rtp$prm.rp$verbose) cat('\ncurrent.observation.time=',as.character(current.observation.time),
                                'rtp$previous.observation.time=',as.character(rtp$previous.observation.time))
    
    if (current.observation.time>rtp$previous.observation.time) {
      # Add latest observations to previous data here
      if (!is.null(rtp$dt.all)) {
        if (ncol(rtp$dt.all)==ncol(dt.sql)) {
          # Test for any mismatching values with overlapping records
          select.current <- which(dt.sql$time %in% rtp$dt.all$time)
          select.previous <- which(rtp$dt.all$time %in% dt.sql$time)
          
          # Count any mismatching or missing values with overlapping records
          mismatchNACount <- sum(dt.sql[select.current,] != rtp$dt.all[select.previous,],na.rm=T) +
            sum(is.na(dt.sql[select.current,])) +
            sum(is.na(rtp$dt.all[select.previous,]))
          
          if (rtp$prm.rp$verbose) {
            cat('\nmerging dt.sql with rtp$dt.all with ',length(select.previous),' overlapping records.',
                'mismatchNACount=',mismatchNACount,'\n')
            if (mismatchNACount>0) {
              for (i in 1:ncol(dt.sql)) {
                if (sum(dt.sql[select.current,i] != rtp$dt.all[select.previous,i],na.rm=T)>0 |
                    sum(is.na(dt.sql[select.current,i]))>0 |
                    sum(is.na(rtp$dt.all[select.previous,i])>0)) {
                  cat('\nmismatch for ',colnames(dt.sql)[i],'=',sum(dt.sql[select.current,i] != rtp$dt.all[select.previous,i],na.rm=T),
                      ' dt.sql_NAs=',sum(is.na(dt.sql[select.current,i])),' rtp$dt.all_NAs=',sum(is.na(rtp$dt.all[select.previous,i])),
                      ' times=(',substr(dt.sql[select.current,'time'][which(dt.sql[select.current,i] != rtp$dt.all[select.previous,i])],12,19),')',
                      ' values=(',dt.sql[select.current,i][which(dt.sql[select.current,i] != rtp$dt.all[select.previous,i])],') vs (',
                      rtp$dt.all[select.previous,i][which(dt.sql[select.current,i] != rtp$dt.all[select.previous,i])],')')
                }
              }
            }
          }
          # merge dt.sql into rtp$dt.all, any overlapping records taken from dt.sql
          rtp$dt.all <- rbind(rtp$dt.all[-select.previous,],dt.sql)
          # drop trailing rtp$dt.all records beyond rtp$prm.pt$rowPastSpan
          if (nrow(rtp$dt.all)>(rtp$prm.pt$rowPastSpan+round(rtp$prm.rp$SQLqueryBufferTime/rtp$prm.pt$dominantTimeInterval))) 
            rtp$dt.all <- rtp$dt.all[(nrow(rtp$dt.all)-rtp$prm.pt$rowPastSpan-round(rtp$prm.rp$SQLqueryBufferTime/rtp$prm.pt$dominantTimeInterval)):nrow(rtp$dt.all),]
        } else if (ncol(dt.sql)<ncol(rtp$dt.all)) {
          cat('\nUNEXPECTED SITUATION rtp$dt.all has columns (',
              colnames(rtp$dt.all)[!colnames(rtp$dt.all) %in% colnames(dt.sql)],') not contained in dt.sql\n')
          mismatchNACount <- mismatchNACount + (ncol(rtp$dt.all)-ncol(dt.sql)) * length(select.current)
        } else { # (ncol(dt.sql)>ncol(rtp$dt.all)
          cat('\nUNEXPECTED SITUATION dt.sql has columns (',
              colnames(dt.sql)[!colnames(dt.sql) %in% colnames(rtp$dt.all)],') not contained in rtp$dt.all\n')
          mismatchNACount <- mismatchNACount + (ncol(dt.sql)-ncol(rtp$dt.all)) * length(select.current)
        }
      } else {
        rtp$dt.all <- dt.sql
        mismatchNACount <- 0
      }
      
      if (rtp$prm.rp$verbose) cat('\nBuilding predictor feature variables.')
      dtb <- tryCatch({
        dbuildPredict(rtp$dt.all,prm.pt=rtp$prm.pt)
      },
      error=function(e) {
        cat('\n\nERRORS in dbuildPredict, UNABLE TO PERFORM PREDICTIONS:\n',e$message,'\n\n')
        dtb <- NULL
      })
      
      if (!is.null(dtb)) { # Only continue processing if dbuild was successful
        dtb <- dtb[,c('time',rtp$prm.pt$cpnames,rtp$prm.pt$tpnames)]
        dtb$time <- as.character(dtb$time) # Needed for complete.cases to work
        dtb <- dtb[complete.cases(dtb),]
        if (rtp$prm.rp$verbose) cat('\nPredictor variables with complete cases ',nrow(dtb),' rows and ',ncol(dtb),' columns.\n\n')
        obsCount <- nrow(dtb)
        dtb <- dtb[nrow(dtb),] # Only process latest observations
        rf.all <- predict(rtp$rf,dtb,predict.all=TRUE)
        dtb$predicted[1] <- rf.all$aggregate[1]
        dtb$rfGTcut[1] <- sum(rf.all$individual>rtp$prm.pt$targetAlarmCutoff)/ncol(rf.all$individual)
        dtb$rfAlarm <- sum(rf.all$individual>rtp$prm.pt$targetAlarmCutoff)/ncol(rf.all$individual)>=rtp$prm.rp$rfGTCutoffAlarm
        
        rtp$dt.output <- rbind(rtp$dt.output,data.frame(time=as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""),
                                                        epoch=as.character(as.numeric(as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""))*1000),
                                                        obsCount=obsCount,
                                                        igbt_tempRunMean=round(dtb$igbt_tempRunMean[1],digits=2),
                                                        rfTempPredict=round(dtb$predicted[1],digits=2),
                                                        rfGTcut=round(dtb$rfGTcut[1],digits=3),
                                                        alarm=dtb$rfAlarm[1],
                                                        ntry=rtp$ntry,
                                                        errorCount=rtp$errorCount,
                                                        mismatch=mismatchNACount,
                                                        processTime=round(as.numeric(difftime(Sys.time(),process.start.time,units='secs')),digits=1),
                                                        lagTime=round(as.numeric(difftime(Sys.time(),as.POSIXlt(dtb$time[1],"%Y-%m-%d %H:%M:%S",tz=""),
                                                                                          units='secs')),digits=1)))
        # Write results to log file
        write.table(rtp$dt.output[nrow(rtp$dt.output),],col.names=if(nrow(rtp$dt.output)==1) T else F,row.names=F)
        rtp$errorCount <- 0
        rtp$ntry <- 0
        
        # Write time epoch and alarm code to database
        if (rtp$prm.rp$sqlWriteAlarmCode) {
          if (rtp$prm.rp$verbose) cat('\nWriting epoch and alarm code to database with sql procedure.\n')
          feedback <- sqlQuery(rtp$ch,paste0("CALL `insert_derived_igbt_alarm_record`('",as.character(rtp$dt.output$epoch[nrow(rtp$dt.output)]),
                                             "','",as.character(if (rtp$dt.output$alarm[nrow(rtp$dt.output)]) 1 else 0),"')"))
          tryCatch({
            if (feedback[1,1] != 1) {
              if (rtp$prm.rp$verbose) {
                cat('\nIrregular database alarm write query feedback:\n')
                print(feedback)
              }
              rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
            }
          },
          error=function(e) {
            if (rtp$prm.rp$verbose) {
              cat('\nIrregular database alarm write query.  Feedback Error follows:\n')
              print(feedback)
              cat('ERRORS in database write query feedback:\n',e$message,'\n')
            }
            rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
          })
        }
        
        # Write time epoch and predicted value to database
        if (rtp$prm.rp$sqlWritePrediction) {
          if (rtp$prm.rp$verbose) cat('\nWriting epoch and predicted value to database with sql procedure.\n')
          feedback <- sqlQuery(rtp$ch,paste0("CALL `insert_derived_igbt_expected_temp`('",as.character(rtp$dt.output$epoch[nrow(rtp$dt.output)]),
                                             "','",as.character(rtp$dt.output$rfTempPredict[nrow(rtp$dt.output)]),"')"))
          tryCatch({
            if (feedback[1,1] != 1) {
              if (rtp$prm.rp$verbose) {
                cat('\nIrregular database rfTempPredict write query feedback:\n')
                print(feedback)
              }
              rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
            }
          },
          error=function(e) {
            if (rtp$prm.rp$verbose) {
              cat('\nIrregular database rfTempPredict write query.  Feedback Error follows:\n')
              print(feedback)
              cat('ERRORS in database write query feedback:\n',e$message,'\n')
            }
            rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
          })
        }
        
        # Write time epoch and rfGTcut value to database
        if (rtp$prm.rp$sqlWriteRFAlarmProp) {
          if (rtp$prm.rp$verbose) cat('\nWriting epoch and rfGTcut value to database with sql procedure.\n')
          feedback <- sqlQuery(rtp$ch,paste0("CALL `insert_derived_igbt_random_forest_vote`('",as.character(rtp$dt.output$epoch[nrow(rtp$dt.output)]),
                                         "','",as.character(rtp$dt.output$rfGTcut[nrow(rtp$dt.output)]),"')"))
          tryCatch({
            if (feedback[1,1] != 1) {
              if (rtp$prm.rp$verbose) {
                cat('\nIrregular database rfGTcut write query feedback:\n')
                print(feedback)
              }
              rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
            }
          },
          error=function(e) {
            if (rtp$prm.rp$verbose) {
              cat('\nIrregular database rfGTcut write query.  Feedback Error follows:\n')
              print(feedback)
              cat('ERRORS in database write query feedback:\n',e$message,'\n')
            }
            rtp$dt.output$errorCount[nrow(rtp$dt.output)] <- rtp$errorCount + 1
          })
        }
        
        # Build diagnostics file if requested
        if (nchar(rtp$prm.rp$outputDiagFilename)>0) {
          rtp$dt.diag <- rbind(rtp$dt.diag,dtb)
        }
        rtp$previous.observation.time <- current.observation.time
      } # !is.null(dtb), no single rig state available
    } # current.observation.time>rtp$previous.observation.time
  } else { # end if (class(tag)=="data.frame")
    if (rtp$prm.rp$verbose) {
      cat('\nProblems with sql query\n')
      print(tags)
    }
    rtp$errorCount = rtp$errorCount + 1
  }
  
  if (sink.number()>0) sink(file=NULL)
  return(rtp)
}