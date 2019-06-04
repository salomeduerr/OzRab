#
# name: reactive_vaccination_hh(goal)
# goal is either "community" or "region" depending on which animals should be vaccinated
# (i.e. all in the infected community or all in the infected region)
#
# This function proceeds the vaccination using the following steps:
# - identifies the dogs that are triggering the vaccination (i.e. rabies detected dogs)
# - selects the vaccination candidates depending on the vaccination goal as those
#   not yet detected nor dead nor vaccinated
# - look up the distances from the triggering dogs to all candidate dogs, ensures
#   that each candidate dog is only listed once with the shortest distance, orders
#   the list according to the distances
# - selects the vacc_capacity/reactive_vacc_cov dogs to be vaccinated today. If not all dogs
#   from a household can be vaccinated today because the vaccination capacity has been
#   reached, the remaining dogs of this household will be vaccinated as well to ensure
#   that the vaccination status of all dogs in the same household is unique
# - undo the vaccination for 1-reactive_vacc_cov of the today vaccinated households (random selection)
# - updates the vaccination status and date in the allDogs-table
#
# called by dayLoop()
#


reactive_vaccination_hh <- function(goal) {

if (verbose) cat("reactive_vaccination_hh() : START\n")

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

  # Check which household has vaccinated and non-vaccinated dogs. This may happen because of three reasons:
  # 1. if some dogs of the vaccinated household are already detected with rabies. Here it is OK that these 
  #    dogs is not vaccinated and it will keep the vaccination status 0 and vaccination date Inf
  # 2. If some dogs of the vaccinates households already died. Again, it is OK that these 
  #    dogs is not vaccinated and it will keep the vaccination status 0 and vaccination date Inf
  # 2. if a vaccinated dog is living at the far away from the triggering dog(s) and it is the last one 
  #    included in the today's capacity of get vaccinated (which is defined by vacc_capacity) then one 
  #    or more other dogs in the same household are out vacc_capacity and will not be vaccinated. In this
  #    case the code below will correct that and will vaccinate all these dogs in that household (which
  #    might excess the vacc_capacity slightly. 
  hhs_vaccDogs_today <- allDogs$hh_ID[vacc_today]                   # vaccinated households today
  hhs_no_vaccination <- allDogs$hh_ID[allDogs$vaccinated == 0]      # non-vaccinated households
  hhs_with_vaccAndNonVacc_dogs <- hhs_vaccDogs_today[which(hhs_vaccDogs_today %in% hhs_no_vaccination)]
  
  allDogs$vaccinated[which(allDogs$hh_ID %in% hhs_with_vaccAndNonVacc_dogs & allDogs$detected == 0 & allDogs$dead == 0)] <<- 1
  allDogs$vaccinationDate[which(allDogs$hh_ID %in% hhs_with_vaccAndNonVacc_dogs & allDogs$detected == 0 & allDogs$dead == 0)] <<- simTIME

  # So far, all dogs closest to the rabid dogs are vaccinated. However, the user can define a
  # percentage of vaccination coverage reached within the region or community. Therefore,
  # after the vaccination of all closest dogs, (1-vaccination coverage) of households will be
  # randomly selected as non-vaccinated. All dogs in these selected households that have been
  # vaccinated today will be defined as non-vaccinated. The dogs in these selected dogs will
  # not get vaccinated at a later stage of the vaccination campaign to guarantee the defined
  # vaccination coverage and will receive a vaccination and protection stati and dates of -9.
  vaccDogs_today <- which(allDogs$vaccinationDate == simTIME)
  hh_vaccDogs_today <- unique(allDogs$hh_ID[vaccDogs_today])

  hh_undo_vaccination <- sample(hh_vaccDogs_today, round((1-reactive_vacc_cov)*length(hh_vaccDogs_today)))

  dogs_undo_vaccination <- which(allDogs$hh_ID %in% hh_undo_vaccination & allDogs$vaccinationDate == simTIME)

  # update the vaccination and protection status of the
  allDogs$vaccinated[dogs_undo_vaccination] <<- -9
  allDogs$vaccinationDate[dogs_undo_vaccination] <<- -9
  allDogs$protected[dogs_undo_vaccination] <<- -9
  allDogs$protectionDate[dogs_undo_vaccination] <<- -9
  
} else {  # if (length(vacc_candidates) > 0)
cat("all animals vaccinated in the", goal, "\n")
}

if (verbose) cat("reactive_vaccination_hh() : START\n")
} # end reactive_vaccination_hh