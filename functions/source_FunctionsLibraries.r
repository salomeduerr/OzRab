#
# name: source_FunctionsLibraries()
# 
# Generic function that sources all *.r files in the current directory
# and loads the required packages
#
# called by the runScript
#

source_FunctionsLibraries <- function(verbose=TRUE) {

if (verbose) cat("source_FunctionsLibraries() : START\n")

# Clear out all previously globally declared variables
rm(list=ls())

# Load the required libraries from the repository library
os <- Sys.info()["sysname"]
if (os == "Linux") {
  libPath <- NULL
} else if (os == "Windows") {
  libPath <- "library"
} else if (os == "Darwin") {
  libPath <- "library"
}

loadStatus_mvtnorm <- require(mvtnorm,lib.loc=libPath)
if (!loadStatus_mvtnorm) stop("ERROR in source_FunctionsLibraries() : library mvtnorm was not found. \n")
else cat("Loaded mvtnorm library\n")

loadStatus_mc2d <- require(mc2d,lib.loc=libPath)
if (!loadStatus_mc2d) stop("ERROR in source_FunctionsLibraries() : library mc2d was not found. \n")
else cat("Loaded mc2d library\n")

loadStatus_sp <- require(sp,lib.loc=libPath)
if (!loadStatus_sp) stop("ERROR in source_FunctionsLibraries() : library sp was not found. \n")
else cat("Loaded sp library\n")

loadStatus_stringr <- require(stringr,lib.loc=libPath)
if (!loadStatus_stringr) stop("ERROR in source_FunctionsLibraries() : library stringr was not found. \n")
else cat("Loaded stringr library\n")

 
# Source files in the relevant directories
source("functions/source_RFiles.r")
sourcingStatus <- source_RFiles("functions/")
return(sourcingStatus)

if (verbose) cat("source_FunctionsLibraries() : END\n")
} # end source_FunctionsLibraries()

