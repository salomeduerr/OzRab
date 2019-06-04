#
# name: within_hh_contacts
#
# This function effects contacts between dogs in the same household in case there            
# is a rabid dog in the household. The probability of a contact depends on the daily
# contact probability of two dogs in the same hh times the probability that a contact 
# will turn into a bite. Only then, when a bite occurred, the contact will be called
# as such (i.e. contact = 1).
#  
# called by dayLoop()
#

within_hh_contacts <- function() {

if (verbose) cat("within_hh_contacts() : START\n")

# define the dogs which are infectious (and not yet dead) and their hhs
infecDogs_sameHH <- which(allDogs$infectious == 1 & allDogs$dead == 0)

# loop through each infectious dog seperately and proceed all contacts between this 
# dog and the dogs in the same household
contactedDogs_WHH <- c()
sapply(infecDogs_sameHH, function (cur_infecDog) {

  hh_cur_infecDog <- allDogs$hh_ID[cur_infecDog]
 
  # candidates that could be contacted by the current infectious dog (cur_infecDog):
  # have to be in the same community, not in the same hh (because those were processed by the
  # within_hh_transmission function) and not yet be exposed
  sameHH_candidates <- which(allDogs$hh_ID == hh_cur_infecDog &
                             allDogs$dog_ID != allDogs$dog_ID[cur_infecDog] &
                             allDogs$exposed == 0 &
                             allDogs$dead == 0)
  
  # draw for each dog a probability that a contact will happen today
  contSameHHDistr <- eval(parse(text=paste("r",cont_prob_sameHH[[1]],sep="")))
  contProbSameHH <-  contSameHHDistr(length(sameHH_candidates),
                      as.numeric(cont_prob_sameHH[[2]]),
                      as.numeric(cont_prob_sameHH[[3]]))

  # draw for each dog a probability that the contact result in a bite (and only then the
  # contact will be called as such (i.e. contact = 1)
  biteSameHHDistr <- eval(parse(text=paste("r",bite_prob_sameHH[[1]],sep="")))
  biteProbSameHH <-  biteSameHHDistr(length(sameHH_candidates),
                      as.numeric(bite_prob_sameHH[[2]]),
                      as.numeric(bite_prob_sameHH[[3]]))
                      
  # draw for the current infected dog a probability that it develops furious rabies after 
  # the development of clinical rabies. In case of this dog does not have the information
  # if he is developing furious rabies, this information will be saved in the allDogs table.
  # Else, in case this information is already stored in the allDogs table, it will be taken
  # from the table.
  furiousDogsDistr <- eval(parse(text=paste("r",prop_furious_dogs[[1]],sep="")))
  propFuriousDogs <-  furiousDogsDistr(1, as.numeric(prop_furious_dogs[[2]]),
                                          as.numeric(prop_furious_dogs[[3]]))
  furious_dog_new <- rbinom(1,1, propFuriousDogs)
  
  if (is.na(allDogs$furious[cur_infecDog])) {
   allDogs$furious[cur_infecDog] <<- furious_dog_new
   furious_dog <- furious_dog_new
   } else { 
   furious_dog <- allDogs$furious[cur_infecDog]
   }
                      
  # define for each dog whether the contact and bite happens today and update the allDogs accordingly
  # the biteProbSameHH is the bite probability with the dog is infectious but without clinical signs
  # this bite probability is increased by increasedBiteProb when a) the dog become clinical and
  # b) the dog develop furious form of rabies
  if (allDogs$clinical[cur_infecDog] == 0 | furious_dog == 0) {
    contSameHH_YN <-  rbinom(length(sameHH_candidates), 1, contProbSameHH*biteProbSameHH)
    } else {
    contSameHH_YN <-  rbinom(length(sameHH_candidates), 1, min(contProbSameHH*biteProbSameHH*increasedBiteProb_sameHH,1)) }
  
  # update the contact, contactDate and contactDog columns in allDogs accordingly
  allDogs$contact[sameHH_candidates[contSameHH_YN==1]] <<- 1
  allDogs$contactDate[sameHH_candidates[contSameHH_YN==1]] <<- simTIME      
  allDogs$contactDog[sameHH_candidates[contSameHH_YN==1]] <<- allDogs$dog_ID[cur_infecDog]
  
  # fill up a vector with the IDs (row numbers) of the contacted dogs
  current_contactedDogs <- allDogs$ID[sameHH_candidates[contSameHH_YN==1]]
  contactedDogs_WHH <<- c(contactedDogs_WHH, current_contactedDogs)
  
}) # end sapply(infecDogs_sameHH)


if (verbose) cat("today's WHH contacts are", length(unique(contactedDogs_WHH)), "\n") 
                    
if (verbose) cat("within_hh_contacts() : END\n")
return(unique(contactedDogs_WHH))
} # end within_hh_contacts

