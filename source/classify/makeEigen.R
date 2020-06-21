##########################################################################
# makeEigen.R - build eigenvector, eigenvalues, and inverse matrix
# Ensign Energy Services Inc. retains all rights to this software
# FHS Mar 20, 2019
##########################################################################
makeEigen <- function(dt,prm.t) {

  eigenRes <- list()
  
  icn <- prm.t$beforeCount + 1 + prm.t$afterCount
  # input multiplier to increase density
  imult <- 4
  istart <- seq(1,icn,ceiling(icn/imult))
  if (length(istart)>imult) istart <- istart[1:(length(istart)-1)]
  
  # Loop through the input predictors for which eigens are to be created
  for (e in which(prm.t$cp$eigencount>0)) {
    if (prm.t$verbose) cat('\n',prm.t$cp$name[e],' eigens with ',icn,' inputs')
    # Initialize input matrix M
    M <- NULL
    # Loop through the distinct drill rig training blocks and builds matrix M
    for (l in levels(dt$Rig)) {
      for (is in 1:length(istart)) {
        inputCol <- dt[dt$Rig==l,prm.t$cp$name[e]]
        inputCol <- inputCol[istart[is]:length(inputCol)]
        # cat('\nRig=',as.character(l),' with istart=',istart[is],' has ',length(inputCol),' records')
        inputCol <- inputCol[1:(floor(length(inputCol)/icn)*icn)]
        # cat(' truncated to ',length(inputCol))
        if (is.null(M)) {
          M <- t(matrix(inputCol,nrow=icn,ncol=length(inputCol)/icn))
        } else {
          M <- rbind(M,t(matrix(inputCol,nrow=icn,ncol=length(inputCol)/icn)))
        }
        # cat(' M now has ',nrow(M),' rows and ',ncol(M),' columns')
      }
    }
    # Sets up the column means matrix with same dimensions as M
    M_colmean <- matrix(data=1,nrow=nrow(M),ncol=1) %*% matrix(colMeans(M),nrow=1,ncol=ncol(M))
    # Computes the covariance matrix C
    C <- t(M) %*% M - t(M_colmean) %*% M_colmean
    eigenC <- eigen(C)
    eigenInv <- solve(eigenC$vectors)
    if (prm.t$verbose) {
      cat(', the first ',prm.t$cp$eigencount[e],' eigenvectors contain ',
          round(100*sum(eigenC$values[1:prm.t$cp$eigencount[e]])/sum(eigenC$values),digits=2),'% of the input sum of squares.')
    }
    eigenRes[[prm.t$cp$name[e]]] <- list(values=eigenC$values,vectors=eigenC$vectors,inverse=eigenInv)
  }

  cat('\n')
  return(eigenRes)
}