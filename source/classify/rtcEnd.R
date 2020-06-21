#######################################################################################
# rtcEnd.R Real Time Rig Classify End V3.0 - close connections, write files
# file also contains function rtcRunLogWrite
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 10, 2019
#######################################################################################
rtcEnd <- function(rtc) {
  
  # Output to log file if requested
  if (nchar(rtc$prm.rt$outputLogFilename)>0) {
    sink(file=rtc$prm.rt$outputLogFilename, append=TRUE)
    options(width=132)
  }
  
  cat('\n\nFinished rig state classifications, actual system time =',as.character(Sys.time()),'\n')
  
  # Write rig classification .csv file if requested (non-blank filename)
  if (nchar(rtc$prm.rt$outputResultFilename) >0) {
    if (nrow(rtc$dt.output)>0) {
      cat('\nWriting RTclassify output file with ',nrow(rtc$dt.output), 'rows and ',ncol(rtc$dt.output), 
          ' columns (csv file) : \n',rtc$prm.rt$outputResultFilename,'\n')
      write.csv(rtc$dt.output,
                file=rtc$prm.rt$outputResultFilename,row.names=FALSE)
    } else {
      cat('\nNo records in RTclassify output file to write... ')
    }
  }
  # write dtb file for Diagnostics
  if (nchar(rtc$prm.rt$outputDiagFilename)>0) {
    if (nrow(rtc$dt.diag)>0) {
      cat('\nWriting RTclassify diagnostic file with ',nrow(rtc$dt.diag), 'rows and ',ncol(rtc$dt.diag), 
          ' columns (csv file) : \n',rtc$prm.rt$outputDiagFilename,'\n')
      write.csv(rtc$dt.diag,
                file=rtc$prm.rt$outputDiagFilename,
                row.names=FALSE)
    } else {
      cat('\nNo records in RTclassify diagnostic file to write...')
    }
  }

  if (nrow(rtc$dt.output)>0) {
    # Print out rig state summary statistics
    rig_state_summary <- as.data.frame(table(rtc$dt.output$rig_state))
    colnames(rig_state_summary) <- c('rig_state','count')
    rig_state_summary$rfvote <- aggregate(rfvote ~ rig_state,rtc$dt.output,mean)[,2]
    rig_state_summary$postProcess <- aggregate(postProcess ~ rig_state,rtc$dt.output,sum)[,2]
    cat('\nRig State Classification Summary Statistics')
    cat('\nrig_state     = final rig state classification')
    cat('\ncount         = count of records with this rig state classification')
    cat('\nrfvote        = average rfvote for this rig state')
    cat('\npostProcess   = count of records where rig state was from post processing\n')
    print(rig_state_summary,row.names=F)
    
    # Print by record summary statistics
    cat('\n\nElapsed time (sec) =',sprintf('%5i',as.integer(rtc$elapsedTime*3600)))
    cat('       Elapsed time since start of program')
    cat('\nInterval Count     =',sprintf('%5i',as.integer(rtc$elapsedTime*3600/rtc$prm.dc$timeInterval)))
    cat('       number of ',rtc$prm.dc$timeInterval,' second intervals in elapsed time')
    cat('\nOutput Records     =',sprintf('%5i',nrow(rtc$dt.output)))
    cat('       Total number of records successfully output with rig state classifications')
    cat('\nSQL read attempts  =',sprintf('%5i',sum(rtc$dt.output$ntry)))
    cat('       Number of SQL calls to "get_drilling_data_between_two_dates"')
    cat('\nSkipped SQL reads  =',sprintf('%5i',sum(rtc$dt.output$nskip)))
    cat('       skipped SQL query calls that were from the same ',rtc$prm.dc$timeInterval,' second interval as the previously saved record')
    cat('\nSQL read errors    =',sprintf('%5i',sum(rtc$dt.output$readErr,na.rm=T)))
    cat('       Count of SQL calls where returned data was not recognizable')
    cat('\nSQL write errors   =',sprintf('%5i',sum(rtc$dt.output$writeErr,na.rm=T)))
    cat('       Count of SQL write calls that returned irregular flag')
    cat('\nIncomplete records =',sprintf('%5i',sum(rtc$dt.output$incomplete,na.rm=T)))
    cat('       Number of records that did not generate complete set of predictors')
    cat('\nMissed intervals   =',sprintf('%5i',sum(rtc$dt.output$missInt,na.rm=T)))
    cat('       Number of ',rtc$prm.dc$timeInterval,' second intervals that were missed and not saved')
    cat('\nMiss-match records =',sprintf('%5i',sum(rtc$dt.output$missMatch>0,na.rm=T)))
    cat('       Number of records where one or more overlapping values from previous record differ by more than',rtc$prm.rt$matchTolerance,'\n')
    
    cat('\nProcessing Time (sec)    min=',sprintf('%5.2f',min(rtc$dt.output$processTime,na.rm=T)),' max=',sprintf('%5.2f',max(rtc$dt.output$processTime,na.rm=T)))
    cat('       processing time for SQL query and rig state determination - should be less than interval=',rtc$prm.dc$timeInterval,'sec') 
    cat('\nLag Time (sec)           min=',sprintf('%5.1f',min(rtc$dt.output$lagTime,na.rm=T)),' max=',sprintf('%5.1f',max(rtc$dt.output$lagTime,na.rm=T)))
    cat('       actual to "current interval" lag, ~ (lag=',rtc$prm.rt$lagDelay,') + (Lookahead+0.5=',
        rtc$prm.t$afterCount+0.5,') * (interval=',rtc$prm.dc$timeInterval,') =',
        rtc$prm.rt$lagDelay+((rtc$prm.t$afterCount+0.5)*rtc$prm.dc$timeInterval),'sec')
    cat('\nLag Time ignored records min=',sprintf('%5i',min(rtc$dt.output$lagdel,na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$lagdel,na.rm=T)))
    cat('       Records ignored within ',rtc$prm.rt$lagDelay,' second lag delay of actual clock time')
    cat('\nRows retrieved           min=',sprintf('%5i',min(rtc$dt.output$nrow,na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$nrow,na.rm=T)))
    cat('       Rows retrieved per record after lag time delete, should be near ',rtc$prm.t$beforeCount,'+ 1 + ',rtc$prm.t$afterCount,
        '=',(rtc$prm.t$beforeCount+1+rtc$prm.t$afterCount))
    cat('\nCols retrieved           min=',sprintf('%5i',min(rtc$dt.output$ncol,na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$ncol,na.rm=T)))
    cat('       Columns retrieved per record, should be constant, i.e. min=max')
    cat('\nOverlapping records      min=',sprintf('%5i',min(rtc$dt.output$noverlap[2:nrow(rtc$dt.output)],na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$noverlap,na.rm=T)))
    cat('       matching time intervals between current and previous SQL retrieval - basis for value match comparison')
    cat('\nNA count before dclean   min=',sprintf('%5i',min(rtc$dt.output$nNAraw,na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$nNAraw,na.rm=T)))
    cat('       Number of missing predictor values from SQL retrieval')
    cat('\nNA count after dclean    min=',sprintf('%5i',min(rtc$dt.output$nNAclean,na.rm=T)),' max=',sprintf('%5i',max(rtc$dt.output$nNAclean,na.rm=T)))
    cat('       Number of missing predictor values after dclean interpolation/extrapolation\n')
    cat('\nPost process reclassifications =',sum(rtc$dt.output$postProcess,na.rm=T))
    cat('       Number records where there was post processing agglomeration rig state changes\n')
    
    
    # Calculate mean and std by predictor
    select.n <- rtc$cumPredStats$n>0
    rtc$cumPredStats$mean <- 0
    rtc$cumPredStats$mean[select.n] <- rtc$cumPredStats$s1[select.n] / rtc$cumPredStats$n[select.n]
    # formula for standard deviation from n, s1, and s2 is sqrt((n/(n-1))*(s2/n-(s1/n)^2))
    select.n <- rtc$cumPredStats$n > 1 & rtc$cumPredStats$min != rtc$cumPredStats$max
    rtc$cumPredStats$std <- 0
    rtc$cumPredStats$std[select.n] <- sqrt((rtc$cumPredStats$n[select.n]/(rtc$cumPredStats$n[select.n]-1))*
                                           (rtc$cumPredStats$s2[select.n]/rtc$cumPredStats$n[select.n]-
                                            (rtc$cumPredStats$s1[select.n]/rtc$cumPredStats$n[select.n])^2))  
  
    cat('\n\nSummary predictor statistics over',nrow(rtc$dt.output),' output records.')
    cat('\nn         = count of SQL query records processed')
    cat('\nmean      = mean (average) value after any UOM conversion')
    cat('\nstd       = standard deviation of values after any UOM conversion')
    cat('\nmin       = minimum value after any UOM conversion')
    cat('\nmax       = maximum value after any UOM conversion')
    cat('\nNAraw     = Count of missing values before dclean interpolation/extrapolation')
    cat('\nNAclean   = Count of missing values after dclean interpolation/extrapolation')
    cat('\nmissMatch = Count of cleaned values between current and previous SQL query that differ by more than ',rtc$prm.rt$matchTolerance)
    cat('\nSQL_UOM   = Incoming UOM from SQL query')
    cat('\nUOM       = Final UOM used in rig state determination if different from incoming UOM')
    cat('\nformula   = UOM conversion formula used\n')
    print(rtc$cumPredStats[,!(colnames(rtc$cumPredStats) %in% c('s1','s2'))],width=132,digits=4)
  } else {
    cat('\nThere are no output records, so there are no summary statistics to produce...\n')
  }
  
  if (sink.number()>0) sink(file=NULL)
  
  return(rtc)
}


#######################################################################################
# rtcRunLogWrite -  Function to write running log, called at end of run
# or if option selected, early from mainRTclassify rtcTest while loop
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Mar 4, 2019
#######################################################################################
rtcRunLogWrite <- function(rtc,start.time) {
  if (nchar(rtc$prm.rt$outputRunLogFilename)>0) {
    # Output to log file if requested
    if (nchar(rtc$prm.rt$outputLogFilename)>0) {
      sink(file=rtc$prm.rt$outputLogFilename, append=TRUE)
      options(width=132)
    }
    # Create run log template
    
    if (nrow(rtc$dt.output)>0) {
      rig_state_summary <- as.data.frame(table(rtc$dt.output$rig_state))
      colnames(rig_state_summary) <- c('rig_state','count')
      rig_state_summary$rfvote <- aggregate(rfvote ~ rig_state,rtc$dt.output,mean)[,2]
    
      rtc$runLog <- data.frame(rig=as.character(rtc$prm.rt$ODBCConnectName),
                               startTime=as.POSIXlt(start.time,origin="1970-01-01 00:00:00"),
                               elapsedHr=round(rtc$elapsedTime,digits=3),
                               gapHr=0,
                               nrec=nrow(rtc$dt.output),
                               nrig_states=nrow(rig_state_summary),
                               top_rig_state=rig_state_summary$rig_state[which(rig_state_summary$count==max(rig_state_summary$count)[1])],
                               count=max(rig_state_summary$count)[1],
                               rfvote=round(rig_state_summary$rfvote[which(rig_state_summary$count==max(rig_state_summary$count)[1])],digits=3),
                               ntry=sum(rtc$dt.output$ntry),
                               readErr=sum(rtc$dt.output$readErr),
                               nskip=sum(rtc$dt.output$nskip),
                               incomplete=sum(rtc$dt.output$incomplete),
                               writeErr=sum(rtc$dt.output$writeErr),
                               missInt=sum(rtc$dt.output$missInt))
    } else {
      rtc$runLog <- data.frame(rig=as.character(rtc$prm.rt$ODBCConnectName),
                               startTime=as.POSIXlt(start.time,origin="1970-01-01 00:00:00"),
                               elapsedHr=round(rtc$elapsedTime,digits=3),
                               gapHr=0,
                               nrec=0,
                               nrig_states=0,
                               top_rig_state="None",
                               rfvote=0,
                               ntry=rtc$sql$ntry,
                               readErr=rtc$sql$readErr,
                               nskip=rtc$sql$nskip,
                               incomplete=rtc$sql$incomplete,
                               writeErr=rtc$sql$writeErr,
                               missInt=0)
    }
    
   
    
    if (file.exists(rtc$prm.rt$outputRunLogFilename)) {
      oldRunLog <- read.csv(rtc$prm.rt$outputRunLogFilename,nrows=-1)
      if (rtc$prm.rt$verbose) cat('\nRead existing runlog file ',rtc$prm.rt$outputRunLogFilename,' with ', nrow(oldRunLog),' rows and ',ncol(oldRunLog),' columns.\n')
      # Compare to template
      if (length(colnames(rtc$runLog))==length(colnames(oldRunLog))) { # same number of columns 
        if (sum(colnames(rtc$runLog)==colnames(oldRunLog))==length(colnames(rtc$runLog))) { # column names are identical
          oldRunLog$rig <- as.character(oldRunLog$rig)
          oldRunLog$startTime <- as.POSIXlt(oldRunLog$startTime,origin="1970-01-01 00:00:00")
          # if runlog saved earlier, delete previously saved last row to be replace with more recent data. 
          if (rtc$prm.rt$earlySaveFlag) {
            if (nrow(oldRunLog)>1) {
              rtc$runLog <- rbind(oldRunLog[1:(nrow(oldRunLog)-1),],rtc$runLog)
            } 
          } else {
            rtc$runLog <- rbind(oldRunLog,rtc$runLog)
          }
        }
      }
    }
    
    # Adds current runlog record to any pre-existing records
    if (nrow(rtc$runLog)>1) {
      rtc$runLog$gapHr[nrow(rtc$runLog)] <- round(as.numeric(difftime(rtc$runLog$startTime[nrow(rtc$runLog)],
                                                                      rtc$runLog$startTime[(nrow(rtc$runLog)-1)],units='hours')) - 
                                                  rtc$runLog$elapsedHr[(nrow(rtc$runLog)-1)],digits=3)
    }
    
    round(as.numeric(difftime(Sys.time(),start.time,units='hours')),digits=3)

    cat('\nWriting RTclassify run log output file with ',nrow(rtc$runLog), 'rows and ',ncol(rtc$runLog), 
        ' columns (csv file) : \n',rtc$prm.rt$outputRunLogFilename,'\n\n')
    write.csv(rtc$runLog,
              file=rtc$prm.rt$outputRunLogFilename,row.names=FALSE)
    rtc$prm.rt$earlySaveFlag <- TRUE
    
    if (sink.number()>0) sink(file=NULL)
  }
  return(rtc)
}
