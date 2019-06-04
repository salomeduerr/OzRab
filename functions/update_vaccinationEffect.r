#
# name: update_vaccinationEffect
#
# This function updates the protection status and date of the today vaccinated dogs
# and reduced the susceptibility of those dogs:
# - sets a scheduled date for protection based on the user defind delay from vaccination
#   to detection,
# - updates the protection status and date as soon as this scheduled protection date is 
#   reached; note: if the dog became infectious in the meanwhile, the vaccination has no
#   effect anymore
# - reduces the susceptibility of the today's protected dogs. The degree this reduction
#   depends on the time point of vaccination of the dog in relation to the time point of
#   exposure with rabies.
#
# The reduction of the susceptibility of the dogs affect the chance of turning from 
# an exposed to an infected individual (incorporated in the update_diseaseStatus).
#
# called by dayLoop()
#

update_vaccinationEffect <- function() {

if (verbose) cat("update_vaccinationEffect() : START\n")

## First: update the protection status of the vaccinated dogs

# set a protection date for the today vaccinated dogs based on the user defined period
toDefineProtDate <- which(allDogs$vaccinated == 1 & allDogs$protDateScheduled == Inf)

protectionDelayDistr <- eval(parse(text=paste("r",protectionDelay [[1]],sep="")))
protDelay <- round(protectionDelayDistr(length(toDefineProtDate),
                   as.numeric(protectionDelay [[2]]),
                   as.numeric(protectionDelay [[3]])))

allDogs$protDateScheduled[toDefineProtDate] <<- simTIME + protDelay

# update the protection status for those dogs reaching the protection date today and
# are still not infectious. In case the vaccinated dogs became infectious in the meanwhile,
# the vaccination has no effect anymore.
protectedToday <- which(allDogs$protDateScheduled == simTIME & allDogs$infectious == 0)
allDogs$protected[protectedToday] <<- 1
allDogs$protectionDate[protectedToday] <<- simTIME


## Second: update the susceptibility of the dogs protected today
reduce_susceptibility <- which(allDogs$protectionDate == simTIME)
late_vaccDistr <- eval(parse(text=paste("r",late_vacc [[1]],sep="")))

sapply(reduce_susceptibility, function(x) {

  # check if the dog which is protected today  has already been exposed and if so, if the
  # time between the exposure date and vaccination date is longe than the user defined 
  # "late_vacc" period. If this is the case, the dog will only profit from a reduced
  # vaccination efficacy than normal.  
  late_vacc_x <- round(late_vaccDistr(1, as.numeric(late_vacc [[2]]),
                                         as.numeric(late_vacc [[3]])))
  
  if (allDogs$exposed[x] == 1 & (allDogs$vaccinationDate[x] - allDogs$contactDate[x] >= late_vacc_x)) {

  red_vaccEfficacyDistr <- eval(parse(text=paste("r",reduced_vaccEfficacy [[1]],sep="")))
  red_vaccEff <- red_vaccEfficacyDistr(1, as.numeric(reduced_vaccEfficacy [[2]]),
                                          as.numeric(reduced_vaccEfficacy [[3]]))
  
  allDogs$susceptibility[x] <<- 1 - red_vaccEff 
  
  } else {

  vaccEfficacyDistr <- eval(parse(text=paste("r",vaccEfficacy [[1]],sep="")))
  vaccEff <- vaccEfficacyDistr(1, as.numeric(vaccEfficacy [[2]]),
                                  as.numeric(vaccEfficacy [[3]]))
             
  
  allDogs$susceptibility[x] <<- 1 - vaccEff 
  }
})  
                                                                  
if (verbose) cat("update_vaccinationEffect() : END\n")
} # end update_vaccinationEffect