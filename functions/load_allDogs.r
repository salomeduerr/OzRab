#
# name: load_allDogs
#
# loads the dog tables from all communities and collate them together as a large
# data frame of all dogs
#
# called from initialize_Simulation
#

load_allDogs <- function(folderName, verbose=T) {

if (verbose) cat("load_allDogs() : START\n")

# Source all *.Rdata files in the current (root) directory
nrOfFilesToSource <- length(list.files(folderName,pattern="\\.Rdata$"))
itsOK <<- rep(TRUE, nrOfFilesToSource)

# load all Rdata files in this directory an print if it worked well
fileCounter <- 0
for (fileName in list.files(folderName,pattern="\\.Rdata$")) {

  fileCounter <- fileCounter + 1

  if (verbose) {
    cat("Sourcing: ",folderName,fileName," ...",sep="")
  }

  tryCatch({
    load(file.path(folderName, fileName))
    assign(paste(fileName), dog_table, envir=.GlobalEnv)
  }, condition=function(ex) {
    itsOK[fileCounter] <<- FALSE
  })

  if (itsOK[fileCounter] & verbose) {
    cat(" OK!\n",sep="")
  }
  else if (verbose) {
    cat(" FAILED!\n",sep="")
  }
} # for()

# collate all the dogs from the different communities together
all_Dogs <- data.frame()
sapply (list.files(folderName,pattern="\\.Rdata$"), function(fileName) {
 cur_com <- eval(parse(text=fileName))
 all_Dogs <<- rbind(all_Dogs, cur_com)
 })

if (any(!itsOK)) { stop ( "error in load_allDogs while loading dog tables from the Dogs-folder")
} else {
return(all_Dogs)}

if (verbose) cat("load_allDogs() : END\n")
} # end load_allDogs

