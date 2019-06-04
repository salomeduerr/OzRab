#
# name: cull_contactedDogs()
#
# This function culls the a given percentage of the dogs contacted by a detected
# rabid dog. 
# It first defines the candidate dogs and culls a simulated number of dogs which is
# 1/cullProp_contactedDogs * the daily remaining culling capacity (after culling of the
# rabid detected dogs).
# In a second step it un-does the culling for 1-cullProp_contactedDogs of the today 
# culled dogs (random selection).
# It finally updates the culling and mortatlity status and the date in the allDogs table. 
#
# called by dayLoop()
#

cull_contactedDogs <- function() {

if (verbose) cat("cull_contactedDogs() : START\n")

# identify the detected dogs and extract those dogs which have been contacted by
# the detected dogs and not yet dead and not yet earlier defined to be culled because
detected_dogs <- which(allDogs$detected == 1)
ID_detectedDogs <- allDogs$dog_ID[detected_dogs]

# if the dogs are contacted today (by the same or a new dog) the allDogs$cullContactedDog
# status has to be cleared to zero
sapply(1:nrow(allDogs), function (x) {
 if(allDogs$contactDate[x] == simTIME) allDogs$cullContactedDog == 0
})

# cull_candidates are those dogs contacted by a detected dog, still alive and not yet
# excluded from culling because of the culling proportion during earlier days.
# Therefore, it might also be that these dogs are contacted earlier than today and
# have not yet been culled because the culling capacity has been reached.
# Potentially it can be considered that vaccinated dogs should not been culled.
if (consider_vaccination == "yes") {
  cull_candidates <- which(allDogs$contactDog %in% ID_detectedDogs & allDogs$dead==0
                           & allDogs$cullContactedDog == 0 & allDogs$vaccinated !=1) } 
  else if (consider_vaccination == "no") {
  cull_candidates <- which(allDogs$contactDog %in% ID_detectedDogs & allDogs$dead==0
                           & allDogs$cullContactedDog == 0 ) } 

# definition of dogs to be culled only if there are candidates around
if (length(cull_candidates) > 0) {

  # The today's cull_capacity for the culling of contacted dogs is the original cull-capacity
  # minus the number of dogs culled via cull_detectDogs. The simulated number of dogs
  # culled per day is higher as the remaining cull_capacity because, in the next step,
  # the number of dogs culled will be randomly reduced based on the user defined cullProp_contactedDogs
  remaining_cull_capacity <- cull_capacity - detectDogs_culled_today
  simulated_number_cull_today <- round(remaining_cull_capacity / cullProp_contactedDogs)

  # cull either all or a sample of the dog candiates depending on the today's culling capacity
  if (length(cull_candidates) > simulated_number_cull_today) {
      sim_dogs_cull_today <- sample(cull_candidates, simulated_number_cull_today)
  } else {sim_dogs_cull_today <- cull_candidates }

  # define which of these dogs selected for culling are actually culled based in the
  # cullProp_contactedDogs: sample those not culled and define the other as finally culled
  undo_culling <- sample(sim_dogs_cull_today, round((1-cullProp_contactedDogs)*length(sim_dogs_cull_today)))
  culled_today <- sim_dogs_cull_today[!(sim_dogs_cull_today %in% undo_culling)]

  # update the mortality and culled status and date
  allDogs$dead[culled_today] <<- 1
  allDogs$mortalityDate[culled_today] <<- simTIME
  allDogs$cullContactedDog[culled_today] <<- 1

  # re-set all scheduled days at -9 to ensure that the culled dogs do not play any
  # role anymore in the model
  allDogs$exposed[culled_today] <<- -9
  allDogs$infDateScheduled[culled_today] <<- -9
  allDogs$clinDateScheduled[culled_today] <<- -9
  allDogs$mortDateScheduled[culled_today] <<- -9
  allDogs$detDateScheduled[culled_today] <<- -9

  # update the cullContactedDog status of the dogs kept alive so that they are not anymore
  # candidates for the contact dog culling (unless contacted again)
  allDogs$cullContactedDog[undo_culling] <<- -9

  # keep track of the number of dogs culled due to reactive culling over the entire outbreak
  contactedDogs_culled_today <<- length(culled_today)
  dogs_contactCulled <<- dogs_contactCulled + length(culled_today)

} else {  # if (length(cull_candidates) > 0)
cat("there is no living dog contacted by the detected dogs \n")
}

if (verbose) cat("cull_contactedDogs() : START\n")
} # end cull_contactedDogs