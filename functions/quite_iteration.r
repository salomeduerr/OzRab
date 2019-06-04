#
# name: quite_iteration
#
# checks if there is a reason to quite the iteration (i.e. the run)
# either because max_time is reached of because there is no "dangerous" dog left
# in allDogs. A dog is dangerous if it is exposed, infectious or clinical but not
# yet dead, i.e. 0 < exposed + infectious + clinical + dead < 4 (see below)
#
# called by dayLoop(), once at the start (before the daily loop starts) and 
#                      once every day within the loop 
#

quite_iteration <- function() {

if (verbose) cat("quite_iteration() : START\n")

# set default values
timeToQuit <- FALSE
statusMsg  <- ""

# Quit if there are no infected individuals any more
 # a dog does not play any role for rabies transmission in case 
 # a) it is already dead: and then exposed, infectious, clinical and dead are all = 1
 # b) it is not exposed: and then exposed, infectious, clinical and dead are all = 0
 # therefore, the model has to check if there is any dog around with
 # 0 < exposed + infectious + clinical + dead < 4

if (length(which(allDogs$exposed + allDogs$infectious + allDogs$clinical + allDogs$dead > 0 & 
                 allDogs$exposed + allDogs$infectious + allDogs$clinical + allDogs$dead < 4 ))
           == 0) {      
  timeToQuit <- TRUE
  statusMsg  <- "Termination because there is no infected dog anymore"
} else {

# Quit if the maximum time has been reached
if (simTIME >= max_time) {
  timeToQuit <- TRUE
  statusMsg  <- "Termination because maxTime has been reached"
}}

return( list(timeToQuit=timeToQuit, message=statusMsg) )

if (verbose) cat("quite_iteration() : END\n")
}  # end quite_iteration