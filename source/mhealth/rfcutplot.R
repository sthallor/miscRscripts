###########################################################################
# rfcutplot.R
# plot of random forest vote cutoffs for machine health forescasts
# Ensign Energy Services Inc. retains all rights to this software
# FHS Mar 1, 2017
##########################################################################
rfcutplot <- function(rfcut,title) {
  select <- rfcut$cutoff<1
  rfGT100CutoffAlarm <- rfcut$cutoff[rfcut$f_score==max(rfcut$f_score)]
  # rfGT100CutoffWarning <- rfcut$cutoff[rfcut$f_score2==max(rfcut$f_score2)]
  
  plot(rfcut$cutoff[select],rfcut$sensitivity[select],
       ylim=c(-0.05,1.05),xlim=c(-0.05,1.05),type='l',col='blue',
       main=title,
       xlab='Random Forest Tree Vote Proportion Cutoff',
       ylab='Score')
  lines(rfcut$cutoff[select],rfcut$posPredValue[select],col='green')
  lines(rfcut$cutoff[select],rfcut$f_score[select],col='grey')
  points(rfcut$cutoff[rfcut$cutoff==rfGT100CutoffAlarm],rfcut$sensitivity[rfcut$cutoff==rfGT100CutoffAlarm],col='blue',pch=20)
  points(rfcut$cutoff[rfcut$cutoff==rfGT100CutoffAlarm],rfcut$posPredValue[rfcut$cutoff==rfGT100CutoffAlarm],col='green',pch=20)
  # points(rfcut$cutoff[rfcut$cutoff==rfGT100CutoffWarning],rfcut$sensitivity[rfcut$cutoff==rfGT100CutoffWarning],col='blue',pch=20)
  # points(rfcut$cutoff[rfcut$cutoff==rfGT100CutoffWarning],rfcut$posPredValue[rfcut$cutoff==rfGT100CutoffWarning],col='green',pch=20)
  # warningLine <- matrix(c(rfGT100CutoffWarning,-0.05,
  #                         rfGT100CutoffWarning,1.05),nrow=2,byrow=TRUE)
  # lines(warningLine[,1],warningLine[,2],col='yellow2')
  alarmLine <- matrix(c(rfGT100CutoffAlarm,-0.05,
                        rfGT100CutoffAlarm,1.05),nrow=2,byrow=TRUE)
  lines(alarmLine[,1],alarmLine[,2],col='red')
  
  # legend('bottom',c('sensitivity = 1 - false negatives',
  #                   'positive predictive value = 1 - false positives',
  #                   'f_score = 2*(sens*ppv)/(sens+ppv)',
  #                   'warning rf tree proportion cutoff',
  #                   'alarm rf tree proportion cutoff (max f_score)'),
  #        fill=c('blue','green','grey','yellow2','red'),cex=0.8)
  legend('bottom',c('sensitivity = 1 - false negatives',
                    'positive predictive value = 1 - false positives',
                    'f_score = 2*(sens*ppv)/(sens+ppv)',
                    'alarm rf tree proportion cutoff (max f_score)'),
         fill=c('blue','green','grey','red'),cex=0.8)
}