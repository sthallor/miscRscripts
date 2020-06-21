###########################################################################
# rfcutplot.R
# plot of random forest vote cutoffs for machine health forescasts
# Ensign Energy Services Inc. retains all rights to this software
# FHS April 28, 2017
##########################################################################
rfcutplot <- function(rfcut,title,prm.p) {
  select <- rfcut$cutoff<1
  rfGTCutoffAlarm <- rfcut$cutoff[rfcut$f_score==max(rfcut$f_score)][1]

  plot(rfcut$cutoff[select],rfcut$sensitivity[select],
       ylim=c(-0.05,1.05),xlim=c(-0.05,1.05),type='l',col='blue',
       main=title,
       xlab='Random Forest Tree Vote Proportion Cutoff',
       ylab='Score')
  lines(rfcut$cutoff[select],rfcut$posPredValue[select],col='green')
  lines(rfcut$cutoff[select],rfcut$f_score[select],col='grey')
  points(rfcut$cutoff[rfcut$cutoff==rfGTCutoffAlarm],rfcut$sensitivity[rfcut$cutoff==rfGTCutoffAlarm],col='blue',pch=20)
  points(rfcut$cutoff[rfcut$cutoff==rfGTCutoffAlarm],rfcut$posPredValue[rfcut$cutoff==rfGTCutoffAlarm],col='green',pch=20)
  alarmLine <- matrix(c(rfGTCutoffAlarm,-0.05,
                        rfGTCutoffAlarm,1.05),nrow=2,byrow=TRUE)
  lines(alarmLine[,1],alarmLine[,2],col='red')

  legend('bottom',c(sprintf('alarm rf tree proportion cutoff=%.2f',rfGTCutoffAlarm[1]),
                    sprintf('sensitivity = 1 - false negatives = %.3f',rfcut$sensitivity[rfcut$cutoff==rfGTCutoffAlarm][1]),
                    sprintf('positive predictive value = 1 - false positives = %.3f',
                            rfcut$posPredValue[rfcut$cutoff==rfGTCutoffAlarm][1]),
                    sprintf('f_score = %.2f*(sens*ppv)/(sens+%.2f*ppv) = %.3f',1+prm.p$ppvFscoreFactor,prm.p$ppvFscoreFactor,
                            rfcut$f_score[rfcut$cutoff==rfGTCutoffAlarm][1])),
         fill=c('red','blue','green','grey'),cex=0.8)
}