#######################################################################################
# abb_dw_stats.R - looking at historian data from DR156
# Ensign Energy Services Inc. retains all rights to this software
# FHS, May 4, 2016
#######################################################################################

dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
filename <- '160504_DR156_console.csv'
dt <- read.csv(paste0(dir,filename), nrow=750000)
# shorten the column names
colnames(dt)[2:ncol(dt)] <- substr(colnames(dt)[2:ncol(dt)],23,nchar(colnames(dt)[2:ncol(dt)]))
colnames(dt)[2] <- substr(colnames(dt)[2],8,nchar(colnames(dt)[2]))
colnames(dt)[3] <- substr(colnames(dt)[3],7,nchar(colnames(dt)[3]))
colnames(dt)[4:5] <- substr(colnames(dt)[4:5],11,nchar(colnames(dt)[4:5]))
colnames(dt)[6:7] <- substr(colnames(dt)[6:7],8,nchar(colnames(dt)[6:7]))
colnames(dt)[8:20] <- substr(colnames(dt)[8:20],12,nchar(colnames(dt)[8:20]))

for (i in 2:ncol(dt)) dt[,i] <- as.numeric(dt[,i])

print(data.frame(observations=apply(dt[2:ncol(dt)],2,function(x) length(which(!is.na(x)))),
                 missing=apply(dt[2:ncol(dt)],2,function(x) length(which(is.na(x)))),
                 min=apply(dt[,2:ncol(dt)],2,function(x) min(as.numeric(x),na.rm=T)),
                 median=apply(dt[,2:ncol(dt)],2,function(x) median(as.numeric(x),na.rm=T)),
                 max=apply(dt[,2:ncol(dt)],2,function(x) max(as.numeric(x),na.rm=T)),
                 mean=apply(dt[,2:ncol(dt)],2,function(x) round(mean(as.numeric(x),na.rm=T),digits=1)),
                 sd=apply(dt[,2:ncol(dt)],2,function(x) round(sd(as.numeric(x),na.rm=T),digits=1))))

# 1) produce correlation heat map
heatmap(cor(dt[,2:ncol(dt)],use="complete.obs"),
        scale='none',main='DR156 console Correlation Heatmap')

# 2) Keep only non-redundant (not very highly correlated) variables
data.attributes <- colnames(dt)[2:ncol(dt)]
dateTime <- dt[,1]
dt <- dt[,(colnames(dt) %in% data.attributes)]
# mcor <- cor(dt[,2:ncol(dt)],use="complete.obs")
# heatmap(mcor,scale='none',main='DR156 abb_dw Correlation Heatmap')
title <- sprintf('DR156 console %s to %s',substr(dateTime[1],1,6),
                 substr(dateTime[length(dateTime)],1,6))

# 3 Look at histograms
for (i in 1:ncol(dt)) {
  hist(dt[,i],breaks=100,main=title,xlab=colnames(dt)[i])
}

# 4 look at pair plots with correlations
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
count <- 10000
sampleSelect <- sample(1:nrow(dt),count)
dt1 <- dt[sampleSelect,]
pairs(dt1[,c(1,2,5,6,7)], pch='.', gap=0.2,upper.panel=panel.cor, 
      main=sprintf('%s %i samples cor in upperT',title,count))

# 5 K-means cluster analysis
# Normalize the data scales
dt1 <- dt[complete.cases(dt),]
for (cn in data.attributes) {
  cat('\nLooking at ',cn,' mean=',mean(dt1[[cn]]),' sd=',sd(dt1[[cn]]))
  dt1[[cn]][!is.na(dt1[[cn]])] <- (dt1[[cn]][!is.na(dt1[[cn]])] - 
                                  mean(dt1[[cn]]))/sd(dt1[[cn]])
}
count <- 2000 # sample size for cluster analysis
nstart <- 100   # number of starting clusters
iter.max <- 50 # max number of kmeans iterations
kcount <- c(2:10,12,14,16,18,20,23,26,29,32,35,40)

# Create dataframe for kmean results
kmean.res <- data.frame(centers=kcount,norm=rep(0,length(kcount)),rand=rep(0,length(kcount)))
# Subsample indices of dt1 observations for kmean testing
data.sample <- sample(1:nrow(dt1),count)
# Random data corresponding to dt1 dataset
krand <- matrix(rnorm((length(data.sample)*ncol(dt1))),nrow=length(data.sample),ncol=ncol(dt1))

for (i in 1:length(kcount)) {
  k <- kcount[i]
  cat('\nLooking at kclustering with k=',k,' clusters.')
  ktest <- kmeans(dt1[data.sample,],centers=k,iter.max=iter.max,nstart=nstart)
  kmean.res$norm[i] <- ktest$betweenss/ktest$totss
  ktest <- kmeans(krand,centers=k,iter.max=iter.max,nstart=nstart)
  kmean.res$rand[i] <- ktest$betweenss/ktest$totss
}

plot(kmean.res$centers,kmean.res$norm,type='l',col='red',ylim=c(0,1),
     main=sprintf("DR156 K-means clustering with %i vars & %i samples",ncol(dt1),count),
     ylab="between_cluster_SS/total_SS",
     xlab="Cluster Count")
lines(kmean.res$centers,kmean.res$rand,col='black')
legend('bottomright',c('Drill Data','Random Data'),fill=c('red','black'))

# 6 create table of 0,1 values in common
tableValues <- data.frame(colA=NULL,colB=NULL,
                        Zero_Zero=NULL,One_Zero=NULL,
                        Zero_One=NULL,One_One=NULL)
tableList <- c('dwks_zone_a','dwks_zone_b','dwks_gear_high',
               'dwks_js_hst','dwks_js_lwr','dwks_prk_brk',
               'tugr1_js_dwn','tugr2_js_dwn','wrench_dwn',
               'dwks_aux','dwks_vfd_aud_alrmm',
               'dwks_vfd_fault','dwks_zero_spd')

cat('\n')
for (i in 1:(length(tableList)-1)) {
  colA <- tableList[i]
  for (j in (i+1):length(tableList)) {
    colB <- tableList[j]
    values <- as.vector(table(dt[[colA]],dt[[colB]]))
    if (length(values==4)) {
      tableValues <- rbind(tableValues,
                         data.frame(colA=colA,
                                    colB=colB,
                                    Zero_Zero=round(values[1]/sum(values),digits=3),
                                    One_Zero=round(values[2]/sum(values),digits=3),
                                    Zero_One=round(values[3]/sum(values),digits=3),
                                    One_One=round(values[4]/sum(values),digits=3)))
    }
  }
}
print(tableValues)

###############################################################################################
# abb_dw initial statistics
# 
# dir <- '/Users/Fred.Seymour/Historian_Data/160503_MachineHealth/'
# filename <- '160503_DR156_abb_dw.csv'
# dt <- read.csv(paste0(dir,filename), nrow=100000)
# # shorten the column names
# colnames(dt)[2:ncol(dt)] <- substr(colnames(dt)[2:ncol(dt)],30,nchar(colnames(dt)[2:ncol(dt)]))
# 
# 
# # 1) produce correlation heat map
# heatmap(cor(dt[,2:ncol(dt)],use="complete.obs"),
#         scale='none',main='DR156 abb_dw Correlation Heatmap')
# 
# # 2) Keep only non-redundant (not very highly correlated) variables
# data.attributes <- c('igbt_temp','pp_temp1','rpm_ref',
#                      'output_voltage','ctrl_board_temp',
#                      'dc_bus_voltage','fan_on_time',
#                      'current','torque','power',
#                      'frequency','ro1')
# dateTime <- dt[,1]
# dt <- dt[,(colnames(dt) %in% data.attributes)]
# mcor <- cor(dt[,2:ncol(dt)],use="complete.obs")
# heatmap(mcor,scale='none',main='DR156 abb_dw Correlation Heatmap')
# title <- sprintf('DR156 abb_dw %s to %s',substr(dateTime[1],1,6),
#                  substr(dateTime[length(dateTime)],1,6))
# 
# # 3 Look at histograms
# for (i in 1:ncol(dt)) {
#   hist(dt[,i],breaks=100,main=title,xlab=colnames(dt)[i])
# }
# 
# 
# # 4 look at pair plots with correlations
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
# {
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y,use="complete.obs"))
#   txt <- format(c(r, 0.123456789), digits = digits)[1]
#   txt <- paste0(prefix, txt)
#   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }
# count <- 1000
# sampleSelect <- sample(1:nrow(dt),count)
# dt1 <- dt[sampleSelect,]
# pairs(dt1, pch='.', gap=0.2,upper.panel=panel.cor, 
#       main=sprintf('%s %i samples cor in upperT',title,count))
# 
# # 5 K-means cluster analysis
# # Normalize the data scales
# dt1 <- dt[complete.cases(dt),]
# for (cn in data.attributes) {
#   cat('\nLooking at ',cn,' mean=',mean(dt1[[cn]]),' sd=',sd(dt1[[cn]]))
#   dt1[[cn]][!is.na(dt1[[cn]])] <- (dt1[[cn]][!is.na(dt1[[cn]])] - 
#                                      mean(dt1[[cn]]))/sd(dt1[[cn]])
# }
# count <- 1000 # sample size for cluster analysis
# nstart <- 100   # number of starting clusters
# iter.max <- 50 # max number of kmeans iterations
# kcount <- c(2:10,12,14,16,18,20,23,26,29,32,35,40)
# 
# # Create dataframe for kmean results
# kmean.res <- data.frame(centers=kcount,norm=rep(0,length(kcount)),rand=rep(0,length(kcount)))
# # Subsample indices of dt1 observations for kmean testing
# data.sample <- sample(1:nrow(dt1),count)
# # Random data corresponding to dt1 dataset
# krand <- matrix(rnorm((length(data.sample)*ncol(dt1))),nrow=length(data.sample),ncol=ncol(dt1))
# 
# for (i in 1:length(kcount)) {
#   k <- kcount[i]
#   cat('\nLooking at kclustering with k=',k,' clusters.')
#   ktest <- kmeans(dt1[data.sample,],centers=k,iter.max=iter.max,nstart=nstart)
#   kmean.res$norm[i] <- ktest$betweenss/ktest$totss
#   ktest <- kmeans(krand,centers=k,iter.max=iter.max,nstart=nstart)
#   kmean.res$rand[i] <- ktest$betweenss/ktest$totss
# }
# 
# plot(kmean.res$centers,kmean.res$norm,type='l',col='red',ylim=c(0,1),
#      main=sprintf("DR156 K-means clustering with %i vars & %i samples",ncol(dt1),count),
#      ylab="between_cluster_SS/total_SS",
#      xlab="Cluster Count")
# lines(kmean.res$centers,kmean.res$rand,col='black')
# legend('bottomright',c('Drill Data','Random Data'),fill=c('red','black'))

