#
# name: between_hh_contacts
#
# This function simulates the contacts between each rabid dog today to all other                        
# dogs in the same region but not in the same household. The probability of a contact                    
# depends on the daily contact probability of two dogs in the same region (which 
# is based in the distance kernel) times the probability that a contact will turn into
# a bite. Only then, when a bite occurred, the contact will be called as such (i.e. contact = 1).
#
# Currently, this function allows contacts between different communities (although the
# chance is very very low based on the distance kernel). The model will stop in case a 
# between community contact happens, just to get an idea how often this happens. In case 
# it never happens, the contacts can be restricted to only occur in the same community
# (change the definition of the contact candidates), which will make the model much faster.
#
# The control strategy "Keep dogs inside the yards" is directly included here and 
# - defined those dogs with truncated distance kernel (i.e. those affecte by the control
#   strategy and those not affected by it (non-compliance in the strategy)
# - if affected by the control strategy, the infected dog cannot infect dogs farer away
#   than the user defined maximal distance
#
# Inclusion of different kernels per community using the kernels produced by Emily Hudson, Oct 2017
# The infomation of these kernels are read into the model by look-up tables. The look-up tables
# include the information on the min, mode and max of the pert distribution defining the probabiliy
# of contact depending on the distance between the households of the contacting dogs.
# 
# called by dayLoop()
#

between_hh_contacts_communities <- function() {

if (verbose) cat("between_hh_contacts_communities() : START\n")

# define the dogs which are infectious (and not yet dead) and their communities
infecDogs <- which(allDogs$infectious == 1 & allDogs$dead == 0)

    ##############################################################
    # Inclusion of control strategy "Keep dogs inside the yards" #
    ##############################################################
    
    #  If this control strategy is activated, a given percentage x% of dogs will only roam
    #  with a truncated distance kernel. However, 1-x% do not follow the ban and therefore
    #  roam as normal (x is defined by the compliance of the dog owners to this strategy 
    #  by the parameter compliance_BHHMovs
    if (movementBan_BHH == TRUE & any(allDogs$detectionDate + start_BHHban_delay <= simTIME)) {
    
     # binomial sampling to define which infected dog will be affected by the reduced
     # roaming behaviour (i.e. truncated distance kernel) and which dogs roam as usual
     # because of non-compliance of this strategy
     reduced_roaming <-  rbinom(length(infecDogs), 1, compliance_BHHMovs)
     
     # else, if this control strategy is not (yet) activated...
     } else {
     
     # no dog is affected by this strategy (i.e. all reduced_roaming are 0)
     reduced_roaming <- rep(0, length(infecDogs)) }
     
    # collate the infected dogs with the maximal distances in a data frame
    infecDogs_table <- data.frame(infecDogs_ID = infecDogs, redRoam = reduced_roaming)


# loop through each infectious dog seperately and proceed all contacts between this dog and the dogs
# in the same community
contactedDogs_BHH <- c()
sapply(infecDogs, function (cur_infecDog) {

  comm_cur_infecDog <- allDogs$community[cur_infecDog]
  lat_cur_infecDog <-  allDogs$Lat[cur_infecDog]
  long_cur_infecDog <-  allDogs$Long[cur_infecDog]
  hh_cur_infecDog <- allDogs$hh_ID[cur_infecDog]
  
  # allocate the distance kernel (DK) and movement restriction values of the infected dog's 
  # community to the parameters
# Changes made by Emily Oct 2017: We do not need the following 3 lines for the new kernels
#  DK_intercept    <- communityDKmovements[communityDKmovements$community == comm_cur_infecDog, "DK_intercept"]
#  DK_coef         <- communityDKmovements[communityDKmovements$community == comm_cur_infecDog, "DK_coef"]
#  DK_se           <- communityDKmovements[communityDKmovements$community == comm_cur_infecDog, "DK_se"] 
  max_dist_BHHban <- communityDKmovements[communityDKmovements$community == comm_cur_infecDog, "max_dist_BHHban"] 

  # Convert the information of whether the current infected dog is affected by a movement 
  # restrictions. If it is (redRoam = 1), cur_max_dist will be set at max_dist_BHHban,
  # otherwise the cur_max_dist = Inf
  if (infecDogs_table$redRoam[infecDogs_table$infecDogs_ID == cur_infecDog] == 1) {
     cur_max_dist <- max_dist_BHHban } else {
     cur_max_dist <- Inf } 

  # candidates that could be contacted by the current infectious dog (cur_infecDog):
  # have to be in the same region, not in the same hh (because those were processed by the
  # within_hh_transmission function) and have not yet been exposed
  region_cur_infecDog <- allDogs$region[cur_infecDog]
  contact_candidates <- which(allDogs$region == region_cur_infecDog &
                              allDogs$hh_ID != hh_cur_infecDog &
                              allDogs$exposed == 0 &
                              allDogs$dead == 0)

  # look up the distances in the distance matrix between the currently infected dog and the all candicate dogs 
  distances <- as.numeric(distance_matrix[hh_cur_infecDog,allDogs$hh_ID[contact_candidates]])

## calculate the distances between the current infectious dog and all candidate dogs 
#  (this was the older part when the calculation of the distances was done every day) 
#  sapply(contact_candidates, function (x) {
#
#   long_x <- allDogs$Long[x]
#   lat_x <-allDogs$Lat[x]
#   
#   cur_distance <- round(spDistsN1(cbind(long_x, lat_x), c(long_cur_infecDog, lat_cur_infecDog),
#                                   longlat=TRUE),
#                         digits = 3) *1000    # spDistsN1 output for longlat=T is km
#   distances <<- c(distances, cur_distance)
#   })
 
 
  # Define for each of the candidate dog the probability that it will be contacted by the current
  # infectious dog. This probability is defined by the distance kernel with the equation:
  # pc = 1/(1 + exp (-(intercept +  dist_coef * x) while x is the distance between the two dogs's hh.
  # The actual probability is sampled from a pert distribution with the parameters min =lower CL of the
  # distance kernel for x, mode= fitted value via distance kernel, max= upper CL of the kernel.
  # Based on this probability, sample from a binomial distribution if the contact actually happens and return
  # the contacts as a vector with contact YN (contWithinCommToday_YN) for each dog.

  contWithinCommToday_YN <- c()
  sapply(distances, function (x) {
 
    # first check if the current distance is larger than the max_dist of the current 
    # infected dog. If so, the contact does not occur (cur_contWithinCommToday_YN = 0); 
    # otherwise the distance kernel is used 
    if (x > cur_max_dist) {
      cur_contWithinCommToday_YN <- 0
    } else {  

    # update using the community specific kernels produced by Emily Oct 2017:
    # these look-up tables already include information about the min, mode and max
    # of the probability of contact used for the pert distribution
  
    if (comm_cur_infecDog == "Bamaga") kernel_table <- Bamaga_kernel
    if (comm_cur_infecDog == "Seisia") kernel_table <- Seisia_kernel
    if (comm_cur_infecDog == "NewMapoon") kernel_table <- NewMapoon_kernel
    if (comm_cur_infecDog == "Umagico") kernel_table <- Umagico_kernel
    if (comm_cur_infecDog == "Injinoo") kernel_table <- Injinoo_kernel
  
  # the kernel_tables are only up to 400m, for longer distances all probabilities
  # are defined to be 0.
    if (x > 400){
    
      minProbContact <- 0
      maxProbContact <- 0
      mostProbContact <- 0
   } else { 
       
      minProbContact <- kernel_table[kernel_table[,"Distance"]== x,"Min"]
      maxProbContact <- kernel_table[kernel_table[,"Distance"]== x,"Max"]
      mostProbContact <- kernel_table[kernel_table[,"Distance"]== x,"Median"]
    }
    
  #  minProbContact <- 1/(1 + exp (-(DK_intercept +  (DK_coef-1.96*DK_se) * x )))
  #  maxProbContact <- 1/(1 + exp (-(DK_intercept +  (DK_coef+1.96*DK_se) * x )))
  #  mostProbContact <- 1/(1 + exp (-(DK_intercept +  DK_coef * x )))                 
 
 # rpert is not working in case min, mode and max = 0, so we have to directly set the 
 # contProbBetweenHH = 0
   if (minProbContact == 0 & maxProbContact == 0 & mostProbContact == 0) {
    contProbBetweenHH <- 0
   
      } else {
    # draw a probability that a contact will happen today
    contProbBetweenHH <-  rpert(1, minProbContact, mostProbContact, maxProbContact)
      }
    # draw a probability that the contact will results into a bite (and only then it 
    # will be called as such (i.e. contact = 1)
    biteBetwHHDistr <- eval(parse(text=paste("r",bite_prob_betweenHH[[1]],sep="")))              
    biteProbBetwHH <-  biteBetwHHDistr(1, as.numeric(bite_prob_betweenHH[[2]]),
                                          as.numeric(bite_prob_betweenHH[[3]]))    

    # sample from a binomial distribution if the contact and bite actually happens
    # the biteProbBetweenHH is the bite probability with the dog is infectious but without clinical signs
    # this bite probability is increased by increasedBiteProb when dogs become clinical 
     if (allDogs$clinical[cur_infecDog] == 0) {
      cur_contWithinCommToday_YN <-  rbinom(1, 1, contProbBetweenHH*biteProbBetwHH)
      } else {
     cur_contWithinCommToday_YN <-  rbinom(1, 1, min(contProbBetweenHH*biteProbBetwHH*increasedBiteProb_betwHH,1))}
    }

    # add up the vector of 0/1 indicating whether the contact happens or not
    contWithinCommToday_YN <<- c(contWithinCommToday_YN, cur_contWithinCommToday_YN)
  }) # end sapply(distances, function (x) 

  # update the contact, contactDate and contactDog columns in allDogs accordingly
  allDogs$contact[contact_candidates[contWithinCommToday_YN==1]] <<- 1
  allDogs$contactDate[contact_candidates[contWithinCommToday_YN==1]] <<- simTIME
  allDogs$contactDog[contact_candidates[contWithinCommToday_YN==1]] <<- allDogs$dog_ID[cur_infecDog]

  # counts contact between communities happens to get an idea how often that happens
  # if this never happens, the contact_candidates can be defined as in the same community (makes the
  # model much faster).
   if (sum(contWithinCommToday_YN) > 0 &
       any(allDogs$community[contact_candidates[contWithinCommToday_YN==1]] %in% comm_cur_infecDog) == F) {
       betwComm_kernelMovs_counter <<- betwComm_kernelMovs_counter + 1 } 

  # fill up a vector with the IDs (row numbers) of the contacted dogs
  current_contactedDogs <- allDogs$ID[contact_candidates[contWithinCommToday_YN==1]]
  contactedDogs_BHH <<- c(contactedDogs_BHH, current_contactedDogs)
  
}) # end sapply (infecDogs)

if (verbose) cat("today's BHH contacts are", length(unique(contactedDogs_BHH)), "\n", sep=" ") 

if (verbose) cat("between_hh_contacts_communities() : END\n")
return(unique(contactedDogs_BHH))
} # end between_hh_contacts