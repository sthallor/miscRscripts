# Looking at runs analysis of predictions
# FHS Mar 1 2017

# Setup alarm variable on test data
rfGT100CutoffAlarm <- rfcutTest$cutoff[rfcutTest$f_score==max(rfcutTest$f_score)]
rfGT100CutoffWarning <- rfcutTest$cutoff[rfcutTest$f_score2==max(rfcutTest$f_score2)]
test$rfAlarm <- 'OK'
test$rfAlarm[test$rfGT100>rfGT100CutoffWarning] <- 'Warning'
test$rfAlarm[test$rfGT100>rfGT100CutoffAlarm] <- 'Alarm'
test$rfAlarm <- as.factor(test$rfAlarm)
cat('\nCounts of rfAlarm on test data')
print(table(test$rfAlarm)[c('OK','Warning','Alarm')])


cat('\nCount of Actual IGBT temperatures')
test$presentIgbt[is.na(test$presentIgbt)] <- 'coolp'
print(table(test$presentIgbt)[c('coolp','hotp')])

igbtrun <- data.frame(values=rle(as.vector(test$presentIgbt))$values,
                      lengths=rle(as.vector(test$presentIgbt))$lengths)
igbtrun$from <- 0
igbtrun$to <- 0
igbtrun$to <- cumsum(igbtrun$lengths)
igbtrun$from <- igbtrun$to - igbtrun$lengths + 1
cat('\nCount of IGBT temperature runs')
print(table(igbtrun$values)[c('coolp','hotp')])
cat('\nAgglomerating hotp igbtruns where intervening coolp run is less than ',rowFutureOffset+rowFutureSpan)
test$igbtAgglomerated <- test$presentIgbt
for (i in which(igbtrun$values=='coolp' & igbtrun$lengths<(rowFutureOffset+rowFutureSpan))) {
  test$igbtAgglomerated[igbtrun$from[i]:igbtrun$to[i]] <- 'hotp'
}
igbtrunA <- data.frame(values=rle(as.vector(test$igbtAgglomerated))$values,
                      lengths=rle(as.vector(test$igbtAgglomerated))$lengths)
igbtrunA$from <- 0
igbtrunA$to <- 0
igbtrunA$to <- cumsum(igbtrunA$lengths)
igbtrunA$from <- igbtrunA$to - igbtrunA$lengths + 1
cat('\nCount of agglomerated IGBT temperature runs')
print(table(igbtrunA$values)[c('coolp','hotp')])

# Gather some hotp statistics
igbtrunA$hotp <- 0
igbtrunA$hotpDensity <- 0
igbtrunA$AdvancedAlarmTime <- 0
igbtrunA$AlarmDensity <- 0
for (i in which(igbtrunA$values=='hotp')) {
  igbtrunA$hotp[i] <- sum(test$presentIgbt[igbtrunA$from[i]:igbtrunA$to[i]]=='hotp')
  i1 <- igbtrunA$from[i] - (rowFutureOffset+rowFutureSpan)
  i2 <- igbtrunA$from[i] - 1
  i3 <- if (length(which(test$rfAlarm[i1:i2]=='Alarm'))>0) i1 + min(which(test$rfAlarm[i1:i2]=='Alarm')) - 1 else -1
  if (i3>0) {
    igbtrunA$AdvancedAlarmTime[i] <- round((i2 - i3 + 1)/6,digits=1)
    igbtrunA$AlarmDensity[i] <- round(length(which(test$rfAlarm[i3:i2]=='Alarm'))/(i2-i3+1),digits=3)
  } else {
    igbtrunA$AdvancedAlarmTime[i] <- -5
  }
}
igbtrunA$hotpDensity <- round(igbtrunA$hotp/igbtrunA$lengths,digits=3)

igbtrunA$FalseAlarmDensity <- 0
for (i in which(igbtrunA$values=='coolp')) {
  i1 <- igbtrunA$from[i] + 2*(rowFutureOffset+rowFutureSpan) 
  i2 <- igbtrunA$to[i] - 2*(rowFutureOffset+rowFutureSpan)
  if (i2>i1) igbtrunA$FalseAlarmDensity[i] <- round(length(which(test$rfAlarm[i1:i2]=='Alarm'))/(i2-i1+1),digits=3) else 0
}

print(igbtrunA[igbtrunA$values=='hotp',])
print(igbtrunA[igbtrunA$values=='coolp',])

plot(igbtrunA$lengths[igbtrunA$values=='coolp' & igbtrunA$lengths>(4*(rowFutureOffset+rowFutureSpan))]/6,
          igbtrunA$FalseAlarmDensity[igbtrunA$values=='coolp' & igbtrunA$lengths>(4*(rowFutureOffset+rowFutureSpan))],
          xlim=c(0,500),ylim=c(0,1),col='grey',
          xlab='Run Lengths (minutes)',
          ylab='Alarm Density (prop of alarm observations)',
          main='Run Length versus Alarm Density')
points(igbtrunA$lengths[igbtrunA$values=='hotp']/6,igbtrunA$AlarmDensity[igbtrunA$values=='hotp'],col='red')
legend('topright',c('real alarm density vs hot run lengths','false alarm density vs cold run lengths'),fill=c('red','grey'))

# plot(igbtrunA$lengths[igbtrunA$values=='hotp']/6,
#      igbtrunA$AdvancedAlarmTime[igbtrunA$values=='hotp'],
#      xlim=c(0,500),col='red',
#      xlab='Hot IGBT Run Length (minutes)',
#      ylab='Advanced Alarm Time (minutes)',
#      main='Run Length versus Advanced Alarm Time')

hist(igbtrunA$AdvancedAlarmTime[igbtrunA$values=='hotp'],breaks=(5+(rowFutureOffset+rowFutureSpan)/6),
     xlab='Alarm Time in Advance of Overheating Event (minutes)',
     ylab='Overheating Event Count',
     main=sprintf('IGBT Overheating Events Rig156 from %s to %s',substr(test$time[1],1,10),substr(test$time[nrow(test)],1,10)))
