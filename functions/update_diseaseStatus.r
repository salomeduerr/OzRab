#
# name: update_diseaseStatus
#
# updates the disease status (i.e. infectious, clinical, dead) of all dogs in allDog
# it starts with dogs that are exposed but not yet infectious.
# The principle is that for each dog and disease status (infectious, clinical and dead)  
# a scheduled date will first be allocated based on the user defined distribution of the 
# the respective delay (defined in OzRabArguments or in the runScript) and second, if the 
# scheduled date is reached today, the status will be updates (set to 1) as well as the date
# (set to simTIME)
#
# The vaccination effect is incorporated here at the time point when a exposed dog should turn infectious.
# This will only happen in some cases, depending in the susceptibility of the dog. Those dogs
# not turning infectious because they are protected through vaccination, receive an exposed 
# status of -9. 
#
#  called by dayLoop()
#

update_diseaseStatus <- function() {

if (verbose) cat("update_diseaseStatus() : START\n")

# first: define the scheduled infectious day for those dogs which are exposed but do not yet have a
#        scheduled infectious day
toDefineInfectiousDate <- which(allDogs$exposed == 1 & allDogs$infDateScheduled == Inf)

infecDelayDistr <- eval(parse(text=paste("r",infectiousDelay [[1]],sep="")))
#infecDelayParam <- sapply(2:length(infectiousDelay), function(x) {
#                          as.numeric(infectiousDelay [[x]]) })
#length(infectiousDelay)
infecDelay <- round(infecDelayDistr(length(toDefineInfectiousDate),
                    as.numeric(infectiousDelay [[2]]),
                    as.numeric(infectiousDelay [[3]]),
                    as.numeric(infectiousDelay [[4]])))

allDogs$infDateScheduled[toDefineInfectiousDate] <<- simTIME + infecDelay

# second: update the infectious status if the scheduled infectious date is today;
#         this is the position where the model implements the vaccination effects by
#         reducing the chance to become infectious according to the susceptibility of 
#         the inividual
infectiousToday_candidates <- which(allDogs$infDateScheduled == simTIME)

sapply(infectiousToday_candidates, function (x) {
 infect_YN <- rbinom(1, 1, allDogs$susceptibility[x])
 allDogs$infectious[x] <<-  infect_YN
 })
 
infectiousToday <- which(allDogs$infDateScheduled == simTIME & allDogs$infectious == 1)
allDogs$infectiousDate[infectiousToday] <<- simTIME

# Those dogs which don't turn infectious because they are protected through vaccination
# will receive a exposed status of -9
infection_failed_today <- which(allDogs$infDateScheduled == simTIME & allDogs$infectious == 0)
allDogs$exposed[infection_failed_today] <<- -9

# third: define the scheduled clinical day for those dogs which are infectious but do not yet have a
#        scheduled clinical day
toDefineClnicalDate <- which(allDogs$infectious == 1 & allDogs$clinDateScheduled == Inf)

clinicDelayDistr <- eval(parse(text=paste("r",clinicalDelay [[1]],sep="")))
clincDelay <- round(clinicDelayDistr(length(toDefineClnicalDate),
                    as.numeric(clinicalDelay [[2]]),
                    as.numeric(clinicalDelay [[3]])))

allDogs$clinDateScheduled[toDefineClnicalDate] <<- simTIME + clincDelay

# fourth: update the clinical status if the scheduled clinical date is today
clinicalToday <- which(allDogs$clinDateScheduled == simTIME)
allDogs$clinical[clinicalToday] <<- 1
allDogs$clinicalDate[clinicalToday] <<- simTIME


# fifth: define the scheduled mortality day for those dogs which are clincal but do not yet have a
# scheduled mortality day                                       
toDefineMortalityDate <- which(allDogs$clinical == 1 & allDogs$mortDateScheduled == Inf)

mortDelayDistr <- eval(parse(text=paste("r",mortalityDelay [[1]],sep="")))
mortDelay <- round(mortDelayDistr(length(toDefineMortalityDate),
                    as.numeric(mortalityDelay [[2]]),
                    as.numeric(mortalityDelay [[3]]),
                    as.numeric(mortalityDelay [[4]])))

allDogs$mortDateScheduled[toDefineMortalityDate] <<- simTIME + mortDelay

# sixth: update the clinical status if the scheduled clinical date is today
deathToday <- which(allDogs$mortDateScheduled == simTIME)
allDogs$dead[deathToday] <<- 1
allDogs$mortalityDate[deathToday] <<- simTIME

if (verbose) cat("update_diseaseStatus() : END\n")
} # end update_diseaseStatus
