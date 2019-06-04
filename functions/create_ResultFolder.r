#
# name: create_ResultFolder
# 
# creates folders to save the results
# 
# called by initialize_Simulations()
#


create_ResultFolder <- function() {

if(verbose) cat("create_ResultFolder() : START\n")

# First: Create the category directory. It may already exists (which is ok)
#        so checking for error codes returned by dir.create is of limited usefulness
#        here. If the directory fails to be created (for other reasons than already
#        existing) the lower level directories will fail as well 

# set the path of the category directory
resultsPath <<- paste("results/",results_folder,sep="")

# creats the directory (if not yet existing)
options(warn=-1)     # turns off the warning messages
directoryCreatedSucessfully = dir.create(resultsPath, recursive=TRUE)
if (!directoryCreatedSucessfully)
  cat("FYI : Could not create:",resultsPath,", perhaps it already exists. Proceeding....\n")
else
  cat("Created new category directory:",resultsPath,"\n")
options(warn=0)      # re-turns on the warning messages


# Second: Define a time stamp for the current simulation declared in the parent scope.
#         Make also sure that the time stamp is unique, otherwise try to create
#         another time stamp..., repeat until it is unique.
#         At the same time, create the main results directory 
 
timeStampTemplate <<- paste(format(Sys.time(), "%y.%m.%d-%H.%M"))
timeStampExists <- TRUE
howManyIvoryBillsDoYouSee <- 1 # Fudge factor to make sure the time stamp is unique

while(timeStampExists) {
  
  # set the current time stamp and check if this name (or directory) already exists
  timeStamp <<- paste(timeStampTemplate, howManyIvoryBillsDoYouSee, sep="-")
  timeStampExists <- file.exists(paste("results",results_folder,timeStamp,sep="/"))
  
  # in case it already exists, increase the counter by one, otherwise set the path name
  # and create the directory
  if(timeStampExists) {
    
    cat(timeStamp, " exists, incrementing counter and trying again...\n")
    howManyIvoryBillsDoYouSee <- howManyIvoryBillsDoYouSee + 1 
  
  } else {
  
  # set the path of the main results directory
  path <<- paste(resultsPath,timeStamp,sep="/")
  
  # Create the main result directory
  directoryCreatedSucessfully = dir.create(path)
  if (!directoryCreatedSucessfully)
    stop("ERROR in create_ResultFolder() : Could not create ",path,"... Dying!")
  else
    cat("Created main result directory:",path,"\n")
  }
} # end while loop


# cat the time stamp onto the screen
cat("Time stamp of current run: ",timeStamp,"\n")


# Third: Set the path and create the directory for the epi maps (only if epi maps 
#        are supposed to be saved)
spatialDataPath <<- paste(path,"/epiMaps",sep="")

if (saveEpiMapPDF || saveEpiMapBitmap) {
  directoryCreatedSucessfully = dir.create(spatialDataPath)
  if (!directoryCreatedSucessfully)
    stop("ERROR in create_ResultFolder() : Could not create ",spatialDataPath,"... Dying!")
  else
    cat("Created data directory:",spatialDataPath,"\n")
}

# Fourth: Set the path and create the directory for the contact data files (only if  
#         contact data are supposed to be saved)
contactDataPath <<- paste(path,"/contactData",sep="")

if (save_contacts_YN ) {
  directoryCreatedSucessfully = dir.create(contactDataPath)
  if (!directoryCreatedSucessfully)
    stop("ERROR in create_ResultFolder() : Could not create ",contactDataPath,"... Dying!")
  else
    cat("Created data directory:",contactDataPath,"\n")
}
  
# Fifth: Set the path and create the directory for the movement data files (only if  
#        movement data are supposed to be saved)
movementDataPath <<- paste(path,"/movementData",sep="")

if (save_movements_YN ) {
  directoryCreatedSucessfully = dir.create(movementDataPath)
  if (!directoryCreatedSucessfully)
    stop("ERROR in create_ResultFolder() : Could not create ",movementDataPath,"... Dying!")
  else
    cat("Created data directory:",movementDataPath,"\n")
}  

flush.console() # Flush the print buffer to STDOUT to keep the console current

if(verbose) cat("create_ResultFolder() : END\n")
} # end create_ResultFolder()
