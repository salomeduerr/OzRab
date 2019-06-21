#
# name: between_community_movements
#
# This function executes the spread of rabies between communities within the same region. 
# The following steps are conducted:
# - The dogs of interest are defined as those with exposed = 1, detected = 0, 
#   dead = 0, not yet moved and with a defined scheduled infectious date
# - Definition of whether the dogs will be moved today. There are two types of movements:
#   a) short term movements (usually one day trips, length of stay in the visited community is
#      defined by the parameter visit_period. The probabilities of these movements is 
#      defined by the BC_movements_shortTerm parameter (read in from the csv file).
#   b) permanent movememnts (the dogs stay in the new communities). The probabilities of these movements is 
#      defined by the BC_movements_permanent parameter (read in from the csv file). 
# - For short term movements, the period of time the dogs stay in the new visited community
#   will be selected based on the user defined parameter visit_period. It will be explored if 
#   the dogs will become infectious in the visited community or not, i.e. if s 
#   simTIME + visit_period >= infectious date scheduled; only if so, the dog remains in the 
#   moved_dogs_shortTerm
# - The short term movements and permenant movements are compiled into one table moved_dogs 
# - The name of the visited community will be selected via choose_contact_community()
# - The community, household and coordinated of the moved_dogs will be changed in the allDogs
#   table directly. The new household and its coordinates are randomly chosen from all hhs in
#   the new community. The movement status and date will be updated.
#
# Control strategy "total movement ban between the communities" is included between step 3 and 4
# 
# called by dayLoop()
#

between_community_movements <- function () {

if (verbose) cat("between_community_movements() : START\n")

# First: define which exposed, still alive and non-detected dog move to another community 
#        based on the frequency of dog movements from communities
candidate_dogs <- which(allDogs$exposed == 1 & allDogs$detected == 0 &  
                        allDogs$dead == 0 & allDogs$moved == 0 &
                        allDogs$infDateScheduled < Inf)

# the following should only be executed if there are candidate_dogs today 
if (length(candidate_dogs) > 0) {

  # define the movement probabilities of the candidate dogs today based on the movement
  # frequencies of dogs per community
  shortTerm_movProb <- c()
  permanent_movProb <- c()
  sapply(candidate_dogs, function(x) {
  
    # define the current community and respective movement values for short term and permanent movements
    current_community <- allDogs$community[x]
    movements_shortTerm <- communityDKmovements[communityDKmovements$community == current_community, "BC_movements_shortTerm"]
    movements_permanent <- communityDKmovements[communityDKmovements$community == current_community, "BC_movements_permanent"]
        
    # split the imported value string by comma 
    shortTerm_movs <- str_split(movements_shortTerm,pattern=",")[[1]]
    permanent_movs <- str_split(movements_permanent,pattern=",")[[1]]
        
    # draw a value from the defined distributions for short term and permanent movements
    shortTerm_movDistr <- eval(parse(text=shortTerm_movs[1]))
    cur_shortTermMovProb  <- shortTerm_movDistr(1, as.numeric(shortTerm_movs[2]),
                                                   as.numeric(shortTerm_movs[3]),
                                                   as.numeric(shortTerm_movs[4]))
                                                   
    permanent_movDistr <- eval(parse(text=permanent_movs[1]))
    cur_permanentMovProb  <- permanent_movDistr(1, as.numeric(permanent_movs[2]),
                                                   as.numeric(permanent_movs[3]),
                                                   as.numeric(permanent_movs[4]))
  
    # collate all to one vector for each type of movement
    shortTerm_movProb <<- c(shortTerm_movProb, cur_shortTermMovProb)
    permanent_movProb <<- c(permanent_movProb, cur_permanentMovProb)
  })

  # define which dogs is selected to be moved today based on the probabilities of movements 
  shortTerm_movs_YN <-  rbinom(length(candidate_dogs), 1, shortTerm_movProb)
  moved_dogs_shortTerm <- allDogs[candidate_dogs[shortTerm_movs_YN == 1],, drop=F]
  
  permanent_movs_YN <-  rbinom(length(candidate_dogs), 1, permanent_movProb)
  moved_dogs_permanent <- allDogs[candidate_dogs[permanent_movs_YN == 1],, drop=F]
  
 
  # the following should only be executed if there are movements today 
  if (nrow(moved_dogs_shortTerm) + nrow(moved_dogs_permanent) > 0) {
  
   if (nrow(moved_dogs_shortTerm) > 0) {
    # Second a): Short term movements
    #         Define how long the new dog will stay in the new community and set this
    #         in relation to the scheduled infectious date of these dogs. Only if the 
    #         simTIME + stay_contactComm >= scheduled infectious date, this dog will
    #         transmit rabies to the new community.
    visitPeriodDistr <- eval(parse(text=paste("r",visit_period [[1]],sep="")))
    stay_contactComm <- round(visitPeriodDistr(sum(shortTerm_movs_YN),
                        as.numeric(visit_period [[2]]),
                        as.numeric(visit_period [[3]])))
  
    # the not_dangerous dogs are dogs that will not stay in the visited community until
    # they reach the infectious date. Therefore they will be removed from the moved_dogs
    # table
    not_dangerous <- c()
    sapply(1:sum(shortTerm_movs_YN), function(x) {
      if (simTIME + stay_contactComm[x] < moved_dogs_shortTerm$infDateScheduled[x])
      not_dangerous <<- c(not_dangerous, x)
    })
    
    if(length(not_dangerous) > 0) moved_dogs_shortTerm <- moved_dogs_shortTerm[-not_dangerous,]
   } # end (if (nrow(moved_dogs_shortTerm) > 0)
   
    # Second b): Permanent movements
    #            The permanent movements remain in the new community anyway. The total of moved 
    #            dogs are therefore the combination of the short term moved dogs and the permanently
    #            moved dogs. 
    #            Delete duplicated entires (in case a dog was chosen for short term and permanent 
    #            movements today.   
  
    moved_dogs <- rbind(moved_dogs_shortTerm, moved_dogs_permanent)
    duplicates <- which(duplicated(moved_dogs$dog_ID))
    if (length(duplicates) > 0) moved_dogs <- moved_dogs[-duplicates,]
      
      ######                                                                   ######
      # Inclusion of control strategy "total movement ban between the communities"  #
      ######                                                                   ######
      
      #  If this control strategy is activated, the number of movements will be reduced
      #  by a given percentage, reflecting the compliance of the dog owners to this strategy
      
      if (movementBan_BC == TRUE & any(allDogs$detectionDate + start_BCban_delay <= simTIME)) {
    
        # binomial sampling to define which movement will be banned based in the compliance
        # of the dog owners to follow the ban
        undo_movements <-  rbinom(nrow(moved_dogs), 1, compliance_BCMovs)
        
        # remove those banned movements from the moved_dogs table 
        if(sum(undo_movements) > 0) { 
          moved_dogs <- moved_dogs[-which(undo_movements==1),]}
      }
  
    
    # Third: For those dogs still being in the moved_dogs table (i.e. staying at the 
    #        visited community longer than it takes to get infectious), select the 
    #        visited community based on the between community probability matrix
    comms_moved_dogs <- moved_dogs[,"community"]
    contactCommunities <- choose_contact_community(comms_moved_dogs)
    
    
    # Fourth: Update the entries of the moved dogs in the allDogs table. It is assumed 
    #         that the dogs moved to another community and turn infectious there will 
    #         not anymore go back to the original community. Therefore, the community
    #         in the allDogs table will be changed to the visited community and their
    #         coordinates set to a randomly selected household in the new community 
    #         (also the hh_ID will be changed accordingly).
    #         Also, the moved status and movement dates will be updated.
    ID <- moved_dogs[,"ID"] 
    allDogs$community[ID] <<- contactCommunities
    allDogs$moved[ID] <<- 1
    allDogs$movementDate[ID] <<- simTIME
    
    sapply(ID, function(x) {
     
     current_com <- str_sub(allDogs$community[x],1,2)
     new_hh_candidates <- allDogs$hh_ID[str_sub(allDogs$hh_ID,1,1) == current_com]
     
     newHH <- sample(new_hh_candidates, 1)
     newLat <- unique(allDogs$Lat[allDogs$hh_ID == newHH])
     newLong <- unique(allDogs$Long[allDogs$hh_ID == newHH])
    
     allDogs$hh_ID[x] <<- newHH
     allDogs$Lat[x] <<- newLat
     allDogs$Long[x] <<- newLong
     
     if(str_sub(allDogs$hh_ID[x],1,1) == str_sub(allDogs$dog_ID[x],1,1)) 
        stop("error in between_community_movements(), new household allocation failed")
    }) 
     
    # keep the origin community, hh and coordinates of the moved dogs for recording purposes
    # from the moved_dog table (changes have only be done in the allDogs table directly)
    origin_com <<- moved_dogs[,"community"] 
    origin_hh <<- moved_dogs[,"hh_ID"] 
    origin_Lat <<- moved_dogs[,"Lat"] 
    origin_Long <<- moved_dogs[,"Long"] 
  
  }  else {# end if (nrow(moved_dogs_shortTerm) + nrow(moved_dogs_permanent > 0)
  cat ("no between community contacts today \n")
  }
} # end if (length(candidate_dogs) > 0)

if (verbose) cat("between_community_movements() : END\n")
} # end between_community_movements()
