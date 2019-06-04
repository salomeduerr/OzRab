#
# name: display_statusMessage 
#
# This function prints the status messages per day 
# 
# called by dayLoop() at the start of the simulation
#        and by wrapUp_day once for each day
#

display_statusMessage <- function() {

if (verbose) cat("display_statusMessage() : START\n")

if (displayStatusMessage) cat("----------\n",statusMessage,sep="")

if (saveStatusMessage) {
  fileName = paste(path,"/statusMessage.txt",sep="")                
  write(statusMessage, fileName, append=TRUE)
}

flush.console() # Flush the print buffer to STDOUT to keep the console current

if (verbose) cat("display_statusMessage() : END\n")

} # end display_statusMessage
