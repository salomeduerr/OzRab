#
# name: update_statusMessage()
# 
# Updates the one line status message
# 
# called by dayLoop() at the start of the simulation
#        and by wrapUp_day once for each day
#

update_statusMessage <- function() {

if (verbose) cat("update_statusMessage() : START\n")

currentTime      <- paste("Day ",simTIME,"(",max_time,")",sep="")
currentDay       <- paste("Week day ",(simTIME%%7+1),sep="")
percentDone      <- paste( round(( (simTIME/max_time)*100),0),"%",sep="")

# Estimate how long each day takes (a proxy for the speed of the simulation)
totoElapsedTime  <- proc.time() - startTime
elapsedTimeLastDayOnly <- totoElapsedTime - previousTotoElapsedTime
previousTotoElapsedTime <<- totoElapsedTime
speed <- paste("Toto ",            round(totoElapsedTime[3],1),
               "sec/",             round((totoElapsedTime[3]/60),1),"min",
               " : Day duration ", round(elapsedTimeLastDayOnly[3],3),
               "sec",sep="")

# Generate new status message
statusMessage <<- NULL
statusMessage <<- paste(percentDone, currentTime, currentDay, speed, sep=" : ")

# Add simulation time stamp
statusMessage    <<- paste(statusMessage, timeStamp ,"\n",sep=" : ")

if (verbose) cat("update_statusMessage() : END\n")
} # end update_statusMessage