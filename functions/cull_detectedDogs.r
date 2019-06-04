#
# name: cull_detectDogs
#
# This function kills rabies detected dogs immediately, i.e. today. If the number of 
# dogs to cull exceeds the culling capacity, the culling of the remaining dogs is 
# postponed to the next day(s).
# Dogs to cull today are defined as detected = 1 and dead = 0.
# It updates the mortality and culled status and date and sets the scheduled mortality 
# date at -9.
#
# called by dayLoop()
#

cull_detectedDogs <- function() {

if (verbose) cat("cull_detectedDogs() : START\n")

# define the dogs that are detected but not yet dead --> those to cull immediately
dogsToCull <- which(allDogs$detected == 1 & allDogs$dead == 0)

# order the dogs to be culled today after their detection date
dogsToCull_table <- data.frame(ID = allDogs$ID[dogsToCull], detectionDate = allDogs$detectionDate[dogsToCull])
dogsToCull_table <- dogsToCull_table[with(dogsToCull_table, order(detectionDate)), ]

# If there are more dogs to cull today than possible (n > culling capacity), the first
# culling capacity number of dogs will be culled today
if (nrow(dogsToCull_table) > cull_capacity) {
  dogs_culled_today <- dogsToCull_table[1:cull_capacity,"ID"]
  } else {
  dogs_culled_today <- dogsToCull_table[,"ID"]
}

# update the mortality and culled status and date
allDogs$dead[dogs_culled_today] <<- 1
allDogs$mortalityDate[dogs_culled_today] <<- simTIME
allDogs$mortDateScheduled[dogs_culled_today] <<- -9
allDogs$cullDetectDogs[dogs_culled_today] <<-1

# keep track of the number of detected dogs culled today and over the entire outbreak
detectDogs_culled_today <<- length(dogs_culled_today)
detectDogs_culled <<- detectDogs_culled + length(dogs_culled_today)

if (verbose) cat("cull_detectedDogs() : END\n")
} # end cull_detectedDogs