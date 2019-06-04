#
# name: reactive_culling(goal)
#
# goal is either "community" or "region" depending on which animals should be culled
# (i.e. all in the infected community or all in the infected region)
#
# This function executes culling of a given percentage of the entire dog population
# in the defined goal (communtiy or region) by the following steps:
# - identifies the dogs that are triggering the culling (i.e. rabies detected dogs)
# - selects the culling candidates depending on the culling goal as those
#   not yet detected nor dead nor asigned earlier for culling
# - look up the distances from the triggering dogs to all candidate dogs, ensures
#   that each candidate dog is only listed once with the shortest distance, orders
#   the list according to the distances
# - selects the culling_capacity/reactiveCull_prop dogs to be culled today; note that the
#   culling capacity here is reduced by the number of dogs culled through culling of 
#   detected dogs and culling of contacted dogs
# - undo the culling for 1-reactiveCull_prop of the today culled dogs (random selection)
# - updates the culling and mortality status and date in the allDogs table
#
# called by dayLoop()
#


reactive_culling <- function(goal) {

if (verbose) cat("reactive_culling() : START\n")

# the detected dogs are triggering which of the other dogs get culled first
trigger_dogs <- which(allDogs$detected == 1)
community_trigger_dogs <- unique(allDogs$community[trigger_dogs])
region_trigger_dogs <- unique(allDogs$region[trigger_dogs])


# select the candidate dogs for culling based on their culling status,
# clinical status and either community or region based on the goal
# Potentially it can be considered that vaccinated dogs should not been culled.
if (goal == "community") {
 
  if (consider_vaccination == "yes") {
  cull_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$cullReactive == 0
                           & allDogs$vaccinated != 1 & allDogs$community %in% community_trigger_dogs) 
  } else if (consider_vaccination == "no") {
  cull_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$cullReactive == 0
                           & allDogs$community %in% community_trigger_dogs) }

} else if (goal == "region") {
  
  if (consider_vaccination == "yes") {
  cull_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$cullReactive == 0
                           & allDogs$vaccinated != 1 & allDogs$region %in% region_trigger_dogs)
  } else if (consider_vaccination == "no"){
  cull_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$cullReactive == 0
                           & allDogs$region %in% region_trigger_dogs) }
}

# definition of dogs to be culled only if there are candidates around
if (length(cull_candidates) > 0) {

  # create a table with distances between the cull candidates and each of the detected
  # dogs and only consider and save the minimum distance for each candidate dog
  dist_table <<- data.frame(dog_ID = NULL, distance = NULL)
  sapply(cull_candidates, function (x) {
   
    cur_dists <- as.numeric(distance_matrix[allDogs$hh_ID[x],allDogs$hh_ID[trigger_dogs]])
    cur_tab_row <- cbind(dog_ID = allDogs$dog_ID[x], distance = min(cur_dists)) 
    dist_table <<- rbind(dist_table, cur_tab_row)
 
  })

  dist_table <- dist_table[with(dist_table, order(distance)), ]

  # the today's cull_capacity for the reactive culling is the original cull-capacity
  # minus the number of dogs culled via cull_detectDogs and via culling of contacted dogs
  # select the first cull_capacity animals of the dist_table as those dogs to be
  # culled today and update their culling and mortality status and date
  # simulated number of dogs culled per day is higher as the cull_capacity because,
  # in the next step, the number of dogs culled will be randomly reduced based on the
  # user defined reactiveCull_prop.
  remaining_cull_capacity <- cull_capacity - detectDogs_culled_today - contactedDogs_culled_today
  simulated_number_cull_today <- round(remaining_cull_capacity / reactiveCull_prop)

  if (nrow(dist_table) > simulated_number_cull_today) {
    dogs_cull_today <- dist_table[0:simulated_number_cull_today, "dog_ID"]
    } else {
    dogs_cull_today <- dist_table[, "dog_ID"]}

  if (length(dogs_cull_today) != length(unique(dogs_cull_today))) {
     stop(" error in reactive_cull, 1 or more dog IDs to be culled are not unique \n")
     }

  cull_today_candidates <- which(allDogs$dog_ID %in% dogs_cull_today)
  
  # define which of these dogs selected for culling are really culled beased in the
  # reactiveCull_prop: sample those not culled and define the other as finally culled
  undo_culling <- sample(cull_today_candidates, round((1-reactiveCull_prop)*length(cull_today_candidates)))
  culled_today <- cull_today_candidates[!(cull_today_candidates %in% undo_culling)]
  
  # update the mortality and culled status and date
  allDogs$dead[culled_today] <<- 1
  allDogs$mortalityDate[culled_today] <<- simTIME
  allDogs$cullReactive[culled_today] <<- 1
  
  # re-set all scheduled days at -9 to ensure that the culled dogs do not play any
  # role anymore
  allDogs$exposed[culled_today] <<- -9
  allDogs$infDateScheduled[culled_today] <<- -9
  allDogs$clinDateScheduled[culled_today] <<- -9  
  allDogs$mortDateScheduled[culled_today] <<- -9
  allDogs$detDateScheduled[culled_today] <<- -9
  
  # update the cullReactive status of the dogs kept alive so that they are not anymore 
  # candidates for the reactive culling
  allDogs$cullReactive[undo_culling] <<- -9
  
  # keep track of the number of dogs culled due to reactive culling over the entire outbreak
  dogs_reactiveCulled <<- dogs_reactiveCulled + length(culled_today)

} else {  # if (length(cull_candidates) > 0)
cat("all animals culled in the", goal, "\n")
}

if (verbose) cat("reactive_culling() : START\n")
} # end reactive_culling