#
# name: initialize_simulation_communities
#
# This file
# - reads the input data from the scenario folder: betweenCommMovementProbs.csv, communityDistanceKernelsMovements.csv
# - establish the allDog data.frame (i.e. all dogs collated together for the non-rabies situation),
# - defines the index dog(s) and updates the allDogs table, 
# - sets simTIME = 0, 
# - creates results folder
#
# For the work of Emily Oct 2017 we need to read in kernel look-up tables for each community
# separately.
# 
# called by OzRab()
#

initialize_simulation_communities <- function () {

if (verbose) cat("initialize_simulation_communities() : START\n")

startTime <<- proc.time() # Start time
simTIME <<- 0 # Initialize time

# Splash screen
cat("sysname: ",   Sys.info()["sysname"],
    ", version:",  Sys.info()["version"],
    ", machine:",  Sys.info()["machine"],
    "\nnodename:", Sys.info()["nodename"],
    ", login:",    Sys.info()["login"],"\n\n",sep="")

flush.console()

cat("Using scenario: ",scenarioDataPath,"\n")

# read in the communityDistanceKernelsMovements file from the scenarioDataPath folder
fileName <- paste("scenario",scenarioDataPath,"communityDistanceKernelsMovements.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  communityDKmovements <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

# read in the kernel look-up tables for each community from the scenarioDataPath folder
# done by Emily Oct 2017
fileName <- paste("scenario",scenarioDataPath,"betweenCommMovementProbs.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  betweenCommMovementProbs <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

# read in the betweenCommMovementProbs file from the scenarioDataPath folder
fileName <- paste("scenario",scenarioDataPath,"Bamaga_kernel.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  Bamaga_kernel <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

fileName <- paste("scenario",scenarioDataPath,"Injinoo_kernel.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  Injinoo_kernel <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

fileName <- paste("scenario",scenarioDataPath,"NewMapoon_kernel.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  NewMapoon_kernel <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

fileName <- paste("scenario",scenarioDataPath,"Seisia_kernel.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  Seisia_kernel <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

fileName <- paste("scenario",scenarioDataPath,"Umagico_kernel.csv",sep="/")
cat("Loading ", fileName,"... ",sep="")
if (file.exists(fileName)) {
  Umagico_kernel <<- read.csv(fileName)
  cat("OK\n")
} else stop("File missing! Quitting")

# read in the distance matrix between each pair of dogs

fileName <- paste("scenario",scenarioDataPath,"distance_matrix.csv",sep="/")
if (file.exists(fileName)) {
  distance_matrix <<- read.csv(fileName, row.names=1)                                
  } else stop("File missing! Quitting")

# load all dog tables and collate them together
fileFolderName <- paste("scenario",scenarioDataPath,"Dogs/",sep="/")
all_Dogs <- load_allDogs(fileFolderName)

# calculate distances between households with dogs in the index region
#distance_matrix <<- calculate_distance_matrix(all_Dogs)

# add further columns to the all_Dogs table which currently represent the non-rabid situation
allDogs <<- cbind(ID= c(1: nrow(all_Dogs)), # ID that equals row number
                 all_Dogs,
                 index_dog=          F,   # T if the dog is an index dog
                 contact=            0,   # 0/1 depening whether the dog had contact with a rabid dog
                 contactDate=        Inf, # date of first ontact with a rabid dog
                 contactDog=         NA,  # dog_ID of the rabid dog with which the dog had contact
                 exposed=            0,   # 0/1 depening whether the dog had been bitten by a rabid dog
                                          # all exposed dogs eventually turn rabid, just a matter of time
                 infectious=         0,   # 0/1 depening whether the dog is infectious or not
                 infDateScheduled=   Inf, # date when the dog should turn infectius (based on the user define time period)
                 infectiousDate=     Inf, # date of the start of infectious period
                 clinical=           0,   # 0/1 depening whether the dog shows clinical signs
                 clinDateScheduled=  Inf, # date when the dog should turn clinical (based on the user define time period)
                 clinicalDate=       Inf, # date of the start of showing clinical signs
                 dead=               0,   # 0/1 depening whether the dog died
                 mortDateScheduled=  Inf, # date when the dog should die (based on the user define time period
                 mortalityDate=      Inf, # date the dog died
                 moved=              0,   # whether the dog has moved to another community and become infectious there
                 movementDate=       Inf, # date of movement of the dogs becoming infectious in the new community
                 detected=           0,   # 0/1 depending on whether the dog has been detected as being rabid
                 detDateScheduled=   Inf, # date when the dog will be detected (based on the user define time period)
                 detectionDate=      Inf, # date the dog is detected as being rabid
                 vaccinated=         0,   # 0/1 depending of whether the dog is vaccinated or not
                 vaccinationDate=    Inf, # date when the dog is vaccinated
                 protected=          0,   # 0/1 depending on whether the dog is protected by vaccination or not
                 protDateScheduled=  Inf, # the date from when onwards the dog will be protected through vaccination
                 protectionDate=     Inf, # the date from when onwards the dog is protected through vaccination
                 susceptibility=     1,   # proportion of susceptibility for rabied (is reduced when the dog is vaccinated) 
                 cullDetectDogs=     0,   # 0/1 depending on whether the dog has been culled after detection of the disease
                 cullContactedDog=   0,   # 0/1 depending on whether the dog has been culled because contacted by a detected dog
                 cullReactive=       0    # 0/1 depending on whether the dog has been culled because of reactive culling
                
   )


# Declaring some variables in the parent scope (used for the dailyNumberReport)
nb_WHHC <<- 0
nb_WHHT <<- 0
nb_BHHC <<- 0
nb_BHHT <<- 0
nb_BCC  <<- 0
nb_BCT  <<- 0

# Declaration of a counter that keeps track on contacts of dogs between communities happen
# through the distance kernel (thus within the function between_hh_contacts)
betwComm_kernelMovs_counter <<- 0

# Declaring culling report variables
detectDogs_culled <<- 0
dogs_contactCulled <<- 0
dogs_reactiveCulled <<- 0

# creation of results folders; first set the paths to NULL to guarantee that they do not exists from 
# previous runs
resultsPath      <<- NULL
path              <<- NULL
spatialDataPath   <<- NULL
create_ResultFolder()

# selection of the index dogs and update of the allDogs table accordingly
define_indexDog()

# Declare and define various countdown variables in the parent scope
# used in update_statusMessage()
previousTotoElapsedTime <<- 0
timeStamp         <<- NULL
   
if (verbose) cat("initialize_simulation_communities() : END\n")
} # end initialize_simulation
