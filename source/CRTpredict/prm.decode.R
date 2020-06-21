#######################################################################################
# prm.decode.R - decode bracket delimited ASCII parameters dataframe
# Ensign Energy Services Inc. retains all rights to this software
# FHS, Jan 5, 2016
#######################################################################################

trim <- function(x) gsub("^\\s+|\\s+$","",x) # function trims any leading and trailing blanks

prm.decode <- function(prm.d) {
  
  if (ncol(prm.d) != 1) {
    stop('ERRORS DECODING prm data ... dataframe must start with exactly one text column')
  }
  
  # Initialize columns
  prm.d[,2] <- 0 # number of bracketed fields for each line
  for (i in 3:12) { # Up to 10 fields per line allowed
    prm.d[,i] <- ""
  }
  names(prm.d) <- c('text','count','v1','v2','v3','v4','v5','v6','v7','v8','v9','v10')
  
  for (i in 1:nrow(prm.d)) {
    lbcount <- 0
    for (j in 1:nchar(prm.d$text[i])) {
      if (substr(prm.d$text[i],j,j)=='[') {
        lbcount <- lbcount + 1
        if (lbcount > 10) {
          cat('FATAL ERROR DECODING prm data ... in line ',i,'\n',prm.d$text[i],
              '\n HAS MORE THANE TEN [] FIELDS IN ONE LINE\n\n')
          stop('ERRORS DECODING prm data ... only up to ten [] backet fields per line')
        }
        startloc <- j+1
      }
      if (substr(prm.d$text[i],j,j)==']') {
        prm.d$count[i] <- prm.d$count[i]+1
        stoploc <- j-1
        if (prm.d$count[i] != lbcount) {
          cat('FATAL ERROR DECODING prm data ... in line ',i,'\n',prm.d$text[i],
              '\n LEFT AND RIGHT BRACKET COUNTS DO NOT MATCH\n\n')
          stop(sprintf('ERRORS DECODING prm data ... [] bracket counts do not match on input prm line\n%s\n',
                       prm.d$text[i]))
        }
        prm.d[i,(prm.d$count[i]+2)] <- trim(substr(prm.d$text[i],startloc,stoploc))
      }
    }
    if (prm.d$count[i] != lbcount) {
      cat('FATAL ERROR DECODING prm data ... in line ',i,'\n',prm.d$text[i],
          '\n LEFT AND RIGHT BRACKET COUNTS DO NOT MATCH\n\n')
      stop(sprintf('ERRORS DECODING prm data ... [] bracket counts do not match on input prm line\n%s\n',
                   prm.d$text[i]))
    }
  }
  return(prm.d)
}
