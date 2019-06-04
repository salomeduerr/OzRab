#
# name: source_RFiles()
#
# Source all the *.r files in a given directory in a safe manner.
#
# folderName : name of directory where the files to be sourced are located
#              i.e. the main folder of the functions
#
# called by source_FunctionsLibraries()
#

source_RFiles <- function(folderName, verbose=TRUE) {

if (verbose) cat("source_RFiles() : START\n")

# Source all *.R files in the current (root) directory
nrOfFilesToSource <- length(list.files(folderName,pattern="\\.r$"))
itsOK <<- rep(TRUE, nrOfFilesToSource)

fileCounter <- 0
for (fileName in list.files(folderName,pattern="\\.r$")) {

  fileCounter <- fileCounter + 1
  if (verbose) {
    cat("Sourcing: ",folderName,fileName," ...",sep="")
  }
  
  tryCatch({
    source(file.path(folderName, fileName))
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

if (any(!itsOK)) {
  failedFiles <- list.files(folderName,pattern="\\.r")[!itsOK]
  cat("\n")
  cat("###############################################################\n")
  cat("# ERROR in source_RFiles()...\n")
  cat("# Failed to source ",sum(!itsOK)," file(s) in ",folderName,": ",sep="")
  cat(failedFiles,"\n",sep=" ")
  cat("#\n# The source you seek\n# cannot be located but\n# endless others exist\n")
  cat("###############################################################\n")
  return(FALSE)

} else {

  return(TRUE)}
  
if (verbose) cat("source_RFiles() : END\n")
} # end source_RFiles
