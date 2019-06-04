#
# name: reactive_vaccination_dog(goal)
# goal is either "community" or "region" depending on which animals should be vaccinated
# (i.e. all in the infected community or all in the infected region)
#
#  This function proceeds the vaccination using the following steps:
# - identifies the dogs that are triggering the vaccination (i.e. rabies detected dogs)
# - selects the vaccination candidates depending on the vaccination goal as those
#   not yet detected nor dead nor vaccinated
# - look  up the distances from the triggering dogs to all candidate dogs, ensures
#   that each candidate dog is only listed once with the shortest distance, orders
#   the list according to the distances
# - selects the vacc_capacity/reactive_vacc_cov dogs to be vaccinated today
# - undo the vaccination for 1-reactive_vacc_cov of the today vaccinated dogs (random selection 
#   from all vaccinated dogs)
# - updates the vaccination status and date in the allDogs-table
#
# called by dayLoop()
#


reactive_vaccination_dog <- function(goal) {

if (verbose) cat("reactive_vaccination_dog() : START\n")

# the detected dogs are triggering which of the other dogs get vaccinates first
trigger_dogs <- which(allDogs$detected == 1)
community_trigger_dogs <- unique(allDogs$community[trigger_dogs])
region_trigger_dogs <- unique(allDogs$region[trigger_dogs])


# select the candidate dogs for vaccination based on their vaccination status,
# detection status and either community or region based on the goal
if (goal == "community") {

  vacc_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$vaccinated == 0 
                           & allDogs$community %in% community_trigger_dogs)

} else if (goal == "region") {
  
  vacc_candidates <- which(allDogs$detected == 0 & allDogs$dead == 0 & allDogs$vaccinated == 0 
                           & allDogs$region %in% region_trigger_dogs)
}

# checks whether there are dogs selected for vaccination
if (length(vacc_candidates) > 0) {

  # create a table with distances between the vacc candidates and each of the detected
  # dogs and only consider and save the minimum distance for each candidate dog
  dist_table <<- data.frame(dog_ID = NULL, distance = NULL)
  sapply(vacc_candidates, function (x) {
   
    cur_dists <- as.numeric(distance_matrix[allDogs$hh_ID[x],allDogs$hh_ID[trigger_dogs]])
    cur_tab_row <- cbind(dog_ID = allDogs$dog_ID[x], distance = min(cur_dists)) 
    dist_table <<- rbind(dist_table, cur_tab_row)
  
  })
  
  dist_table <- dist_table[with(dist_table, order(distance)), ]

  
  # select the first vacc_capacity animals in the table as the dogs to be vaccinated 
  # today and update their vaccination status and date
  # simulated number of dogs vaccinated per day is higher as the vacc_capacity because,
  # in the next step, the number of dogs vaccinated will be randomly reduced based on the
  # user defined vaccination coverage reached.
  simulated_number_vacc_today <- round(vacc_capacity / reactive_vacc_cov)
  if (nrow(dist_table) > simulated_number_vacc_today) {
    dogs_vacc_today <- dist_table[1:simulated_number_vacc_today, "dog_ID"]
    } else {
    dogs_vacc_today <- dist_table[, "dog_ID"]
    }
  
  if (length(dogs_vacc_today) != length(unique(dogs_vacc_today))) { 
     stop(" error in vaccinate_dogs, 1 or more dog IDs to be vaccinated are not unique \n") 
     } 
  
  vacc_today <- which(allDogs$dog_ID %in% dogs_vacc_today)
  allDogs$vaccinated[vacc_today] <<- 1
  allDogs$vaccinationDate[vacc_today] <<- simTIME
  
  
  # So far, all dogs closest to the rabid dogs are vaccinated. However, the user can define a 
  # percentage of vaccination coverage reached within the region or community. Therefore, 
  # after the vaccination of all closest dogs, (1-vaccination coverage) of dogs will be 
  # randomly selected as non-vaccinated. These selected dogs will not get vaccinated 
  # at a later stage of the vaccination campaign.
  all_vacc_today <- which(allDogs$vaccinationDate == simTIME)
  
  undo_vaccination <- sample(all_vacc_today, round((1-reactive_vacc_cov)*length(all_vacc_today)))
  
  allDogs$vaccinated[undo_vaccination] <<- -9
  allDogs$vaccinationDate[undo_vaccination] <<- -9
  allDogs$protected[undo_vaccination] <<- -9
  allDogs$protectionDate[undo_vaccination] <<- -9

} else {  # if (length(vacc_candidates) > 0)
cat("all animals vaccinated in the", goal, "\n")
}

if (verbose) cat("reactive_vaccination_dog() : START\n")
} # end reactive_vaccination_dog