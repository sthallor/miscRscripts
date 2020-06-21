#######################################################################################
# reshape.R - converts 3 or 4 col TagName raw Historian data to 1 column per TagName format
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Feb 15, 2017
#######################################################################################
reshape <- function(rawd,prm.dc) {
  
  if (ncol(rawd) != 3 & ncol(rawd) != 4) {
    stop('FATAL ERROR IN reshape.R... you selected reshape option and incoming raw TagName file must have 3 or 4 columns')
  } else if (ncol(rawd)==3) {
    colnames(rawd) <- c(prm.dc$timeColName,'TagPath','TagValue')
  } else { # must have ncol(rawd)==4
    # Assumes that 4th column is the historian standardUOM$uomColName
    # Sets same name for entire prm.dc$standardUOM$uomColName column 
    prm.dc$standardUOM$uomColName <- prm.dc$standardUOM$uomColName[1]
    colnames(rawd) <- c(prm.dc$timeColName,'TagPath','TagValue',prm.dc$standardUOM$uomColName[1])
  }
  
  dt <- data.frame(DateTime=unique(rawd[[prm.dc$timeColName]]))
  colnames(dt) <- prm.dc$timeColName

  tagPath <- unique(as.character(rawd$TagPath))
  if (prm.dc$saveAll == FALSE) {
    tagPath <- tagPath[tagPath %in% c(prm.dc$valueLimits$name,prm.dc$discrete$name)]
  }
  
  library(plyr)

  if (prm.dc$verbose) cat('\n\nReshaping ',nrow(dt),' unique DateTime observations and ',length(tagPath),
      ' TagPath values into columns.')
  prm.dc$standardUOM$changeFlag <- FALSE
  for (j in 1:length(tagPath)) {
    if (ncol(rawd)==4 & tagPath[j] %in% prm.dc$standardUOM$dColName) {
      # Determines which standardUOM record i_uom applies and sets the uom_colName 
      i_uom <- which(prm.dc$standardUOM$dColName %in% tagPath[j])
      uom_colName <- paste(prm.dc$standardUOM$uomType[i_uom],prm.dc$standardUOM$uomColName[i_uom],sep='_')
      # If this uom_colName is already present, then it is not needed again
      if (uom_colName %in% colnames(dt)) {
        dttemp <- rawd[rawd$TagPath==tagPath[j],c(prm.dc$timeColName,'TagValue')]
        colnames(dttemp) <- c(prm.dc$timeColName,as.character(tagPath[j]))
      } else {
        # First time with uom_colName, copy uom column into dttemp
        dttemp <- rawd[rawd$TagPath==tagPath[j],c(prm.dc$timeColName,prm.dc$standardUOM$uomColName[1],'TagValue')]
        colnames(dttemp) <- c(prm.dc$timeColName,uom_colName,as.character(tagPath[j]))
      }
      if (prm.dc$verbose) cat('\nprm.dc$standardUOM$uomColName[1]=',prm.dc$standardUOM$uomColName[1],' uom_colName=',uom_colName)
      # Now check if any observations need unit conversions
      uomConvertCount <- length(which(dttemp[[uom_colName]]==prm.dc$standardUOM$inputUnit[i_uom]))
      if (uomConvertCount>0 & prm.dc$convertUOM==TRUE) {
        # Any NAs in uomColName are set to inputUnit
        dttemp[[uom_colName]][is.na(dttemp[[uom_colName]])] <- prm.dc$standardUOM$inputUnit[i_uom]
        uomConvertCount <- length(which(dttemp[[uom_colName]]==prm.dc$standardUOM$inputUnit[i_uom]))

        # makes temporary copy of conversion formula and checks for inconsistencies'
        formula <- prm.dc$standardUOM$formula[i_uom]
        if (substr(formula,1,nchar(prm.dc$standardUOM$outputUnit[i_uom])) != prm.dc$standardUOM$outputUnit[i_uom] |
            is.na(grep(prm.dc$standardUOM$inputUnit[i_uom],formula)[1])) {
          if (prm.dc$verbose) cat('\n\ndclean WARNING ',prm.dc$standardUOM$dColName[i_uom],
              ' unit conversion formula ',prm.dc$standardUOM$formula[i_uom],
              ' inconsistent with inputUnit=',prm.dc$standardUOM$inputUnit[i_uom],
              ' and outputUnit=',prm.dc$standardUOM$outputUnit[i_uom])
        } else {
          # Some units in dColName are being converted, sets flag so unit indicator uomColName is also updated
          prm.dc$standardUOM$changeFlag[i_uom] <- TRUE
          # fixes formula with 'output' as a function of 'input'
          formula <- gsub(prm.dc$standardUOM$outputUnit[i_uom],'output',formula)
          formula <- gsub(prm.dc$standardUOM$inputUnit[i_uom],'input',formula)
          # copies input
          input <- dttemp[[as.character(tagPath[j])]][dttemp[[uom_colName]]==prm.dc$standardUOM$inputUnit[i_uom]]
          # calculates output from input+formula
          eval(parse(text=formula))
          # replaces input uoms with output uoms
          dttemp[[as.character(tagPath[j])]][dttemp[[uom_colName]]==prm.dc$standardUOM$inputUnit[i_uom]] <- output
          if (prm.dc$verbose) cat('\nConverted ',uomConvertCount,prm.dc$standardUOM$uomColName[i_uom],
              ' values in ',prm.dc$standardUOM$dColName[i_uom],' from ',
              prm.dc$standardUOM$inputUnit[i_uom],' to ',prm.dc$standardUOM$outputUnit[i_uom],
              ' with ',prm.dc$standardUOM$formula[i_uom])
        }
      }
    } else { 
      dttemp <- rawd[rawd$TagPath==tagPath[j],c(prm.dc$timeColName,'TagValue')]
      colnames(dttemp) <- c(prm.dc$timeColName,as.character(tagPath[j]))
    }

    dt <- join(dt,dttemp,by=prm.dc$timeColName,type='left',match='first')
    dt[,ncol(dt)] <- as.numeric(dt[,ncol(dt)])
    if (prm.dc$verbose) cat('\nMerged ',as.character(tagPath[j]),'into col',ncol(dt),' with ',nrow(dttemp),
        ' observations and ',length(unique(dttemp[,ncol(dttemp)])), 'unique values.')
  }
  # For any dColName columns with unit conversions, update the uomColName unit values accordingly
  for (i in 1:nrow(prm.dc$standardUOM)) {
    if (prm.dc$standardUOM$changeFlag[i]==TRUE) {
      uom_colName <- paste(prm.dc$standardUOM$uomType[i],prm.dc$standardUOM$uomColName[i],sep='_')
      dt[[uom_colName]] <- as.character(dt[[uom_colName]])
      dt[[uom_colName]][dt[[uom_colName]]==prm.dc$standardUOM$inputUnit[i]] <- prm.dc$standardUOM$outputUnit[i]
      dt[[uom_colName]][is.na(dt[[uom_colName]])] <- prm.dc$standardUOM$outputUnit[i]
    }
  }
  dt[,1] <- as.character(dt[,1])
  return(dt)
}