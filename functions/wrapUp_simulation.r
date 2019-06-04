# 
# name: wrapUp_simulation() 
#
# Does some final tasks before quitting the simulation, e.g. final summary of  
# the status of the system, calculate the elapsed time, and clears all  
# global variables 
#
# called by: OzRab()
# 

wrapUp_simulation <- function() {

if (verbose) cat("wrapUp_simulation() : START\n")

standardSummary("final")

# Figure out how long the simulation(s) took
endTime <<- proc.time()
elapsedTime <<- endTime[3]-startTime[3];
cat("\nTotal elapsed time: ",elapsedTime," seconds, ",elapsedTime/simTIME," seconds/simulation day)\n",sep="")

# Remove all global variables 
if (rmGlobalVarsOnTermination) rm(list=ls())
else cat("Iteration terminated without clearing global vars\n")

if (verbose) cat("wrapUp_simulation() : END\n")
} # end wrapUp_simulation
