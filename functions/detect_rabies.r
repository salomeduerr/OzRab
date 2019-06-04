#
# name: detect_rabies
#
# This function executes detection of rabies in dogs started to show clinical signs
# today.
# Similar as for the disease states in the model, each dog receives a scheduled date
# of being detected for rabies. This date is set based on the detection period defined
# by the user. It is assumed that the detection period is longer for the first case
# in the region (defined by the detectPeriod_firstCase) and then decreases for the
# following cases (detectPeriod_secondCases).
# As soon as this scheduled date is reached, the status "detection" will be set at 1
# and the detectionDate will be set at today (simTIME).
#
# called by dayLoop()
#


detect_rabies <- function(detectPeriod) {

if (verbose) cat("detect_rabies() : START\n")

# define the scheduled detection day for the dogs which a) either turned clinical today,
# i.e. do not have yet any detDateScheduled and b) or already have a scheduled detection date
# set before the detection of the first case. For b), the detection delay will be reduced to
# a detection delay using the detectPeriod_secondCases distribution from today onwards
toDefineDetecDate_new <- which(allDogs$clinical == 1 & allDogs$detDateScheduled == Inf)    
toDefineDetecDate_update <- which(allDogs$clinical == 1 & allDogs$detDateScheduled > simTIME + as.numeric(detectPeriod[[4]]))  
toDefineDetecDate <- c(toDefineDetecDate_new, toDefineDetecDate_update)         

detectDelayDistr <- eval(parse(text=paste("r",detectPeriod [[1]],sep="")))
detecDelay <- round(detectDelayDistr(length(toDefineDetecDate),
                    as.numeric(detectPeriod [[2]]),
                    as.numeric(detectPeriod [[3]]),
                    as.numeric(detectPeriod [[4]])))

allDogs$detDateScheduled[toDefineDetecDate] <<- simTIME + detecDelay

# update the detection status if the scheduled detection date is today
detectionToday <- which(allDogs$detDateScheduled == simTIME)
allDogs$detected[detectionToday] <<- 1
allDogs$detectionDate[detectionToday] <<- simTIME

if (verbose) cat("detect_rabies() : END\n")
} # end detect_rabies()