######################################################################
# ftd.R - computes factor time distance
# distance in timespan from current observation group
# to nearest observation group with 'value'
# Ensign Energy Services Inc. retains all rights to this software
# FHS Oct 6, 2015
##########################################################################
ftd <- function(rep, value, maxtime, index, dir='e') {
  # rep - runs report with columns elapsed_minutes (timespan in minutes) and rig_state
  # value - factor value to find in rep$rig_state
  # maxtime - max time (rep$elapsed_minutes) to seek value in rep$rig_state
  # index - index for current position in rep from which to search
  # dir - direction of search, 'e'=either, 'b'=before only, 'a'=after only
  # returns TRUE if found, otherwise returns FALSE
  if (index < 1 | index > nrow(rep)) return(FALSE)
  # Look before index
  if (dir=='e' | dir=='b') {
    i <- index
    timedist <- 0
    while (i > 1 & timedist < maxtime) {
      i <- i-1
      if (!is.na(rep$rig_state[i])) {
        if (rep$rig_state[i] == value) return(TRUE)
      }
      timedist <- timedist + rep$elapsed_minutes[i]
    }
  }
  # Look after index
  if (dir=='e' | dir=='a') {
    i <- index
    timedist <- 0
    while (i <nrow(rep) & timedist < maxtime) {
      i <- i+1
      if (!is.na(rep$rig_state[i])) {
        if (rep$rig_state[i] == value) return(TRUE)
      }
      timedist <- timedist + rep$elapsed_minutes[i]
    }
  }
  # Not found
  return(FALSE)
}