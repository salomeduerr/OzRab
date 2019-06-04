#
# name: dayLoop 
#
# The dayLoop() function loops though the days. This is basically a while loop running as long
# as simTIME hasn't reach the defined maximal simulation time or until there is no infected dog
# anymore present
#
# Activities during the day are:
# - upadte the disease states of the dogs
# - within hh contacts and transmissions
# - between hh contacts and transmissions
# - between community contacts and tranmissions
# - contrl strategies if applicable: vaccination, pre-emptive culling, movement restrictions
#
# called by: OzRab
#

dayLoop <- function () {

if (verbose) cat("dayLoop() : START\n")

terminationStatus <- quite_iteration()
update_statusMessage()
display_statusMessage()  

######################################################
#   Start the day loop for the current simulation    #
######################################################
while (!terminationStatus$timeToQuit) {

  simTIME <<- simTIME + 1
  
  if (simTIME==1) {
  # the name of the summaryFunction is defined in OzRabArguments or in the runScript,
  # currently, there is only one option: standardFunction()
  standardSummary("init")
  }

  # update the epi status (i.e. exposed, infectious, clinical, dead) of all dogs in allDog
  update_diseaseStatus()
  
  # Within household (hh) contacts
  WHHcontacts <- within_hh_contacts()
  
  # Define which within household contact leads to virus transmission
  define_transmission(WHHcontacts)
  
  # Between household (hh), within community contacts --> based on the distance kernel
  BHHcontacts <- between_hh_contacts()
 # BHHcontacts <- between_hh_contacts_communities()  # used for Emily's work Oct 2017
 # BHHcontacts <- between_hh_contacts_categories()  # used for Emily's work Oct 2017

  # Define which between household contact leads to virus transmission
  define_transmission(BHHcontacts)
  
  # between community movement is based on the frequency of dog movements between the communities,
  # the time period the dogs remain in the vistited community and a weighting matrix to select the
  # visted community. Between community movements only happen in regions with more than 1 community
  indexDogs <- which(allDogs$index_dog==T)
  affected_region <- unique(allDogs$region[indexDogs])
  affected_communities <- unique(allDogs$community[which(allDogs$region == affected_region)])
  if (length(affected_communities) > 1) between_community_movements()
  
  # detection of rabies from clinical dogs
  if (any(allDogs$detected == 1)) 
  { detect_rabies(detectPeriod_secondCases) } else {
    detect_rabies(detectPeriod_firstCase) }
 
  
      ######################
      # Control strategies #
      ######################
      
      # Vaccination:
      #
      # Conduct vaccination in case the vaccination goal is set to a given value. Note that
      # the vaccination will result in a reduction of the susceptibility of the dogs, which
      # will affect the chance to turn from an exposed to an infected individual.
      # The vaccination coverage can either be defined on the individual dog level (i.e. a given
      # percentage of dogs are vaccinated (irrespectively of if they are living in the same or 
      # different household; -->  reactVacc_covLevel_reference == "dog" calling reactive_vaccination_dog) 
      # or on the household level (i.e. a given percentage of households will be vaccinated and all 
      # dogs in that hh are vaccinated; --> reactVacc_covLevel_reference == "household" calling 
      # reactive_vaccination_hh)
      if (any(allDogs$detectionDate + start_vacc_delay <= simTIME)) {

        if (react_vaccination == TRUE & reactVacc_covLevel_reference == "dog") { 
            reactive_vaccination_dog(vaccination_goal)
            update_vaccinationEffect() }
       
        if (react_vaccination == TRUE & reactVacc_covLevel_reference == "household")  { 
            reactive_vaccination_hh(vaccination_goal)
            update_vaccinationEffect() }
      }
             
      # Culling: Three methods of culling can be chosen, culling of rabid dogs at the day of detection, culling of
      #          dogs contacted by detected rabid dogs and reactive culling (i.e. culling of a given 
      #          percentage of the population out of the non-detected dogs and non-contacted dogs). The total number
      #          of dogs culled within a day is limited by the daily culling capacity.
      
      # Culling of detected dogs will be conducted if cull_detectedDogs == T and the delay to start this
      # culling is expired
      if (cull_detectDogs == TRUE & any(allDogs$detectionDate + start_cullDetectDog_delay <= simTIME)) {
          cull_detectedDogs() } 
           
      # Culling of dogs that has been contacted by detected rabid dogs
      if (cull_contactDogs == TRUE & any(allDogs$detectionDate + start_cullContactedDog_delay <= simTIME)) {
          cull_contactedDogs() } 
               
      # Reactive culling of undetected dogs will be conducted if reactive_cull == T and the delay to start this
      # culling is expired  
      if (reactive_cull == TRUE & any(allDogs$detectionDate + start_reactiveCull_delay <= simTIME)) {
          reactive_culling(culling_goal)}
  
  
  # the name of the summaryFunction is defined in OzRabArguments or in the runScript,
  # currently, there is only one option: standardFunction()
   standardSummary("day")
  
  # decides whether the iteration should be finished (because the simTIME >= max_time or because there
  # are no infected dogs anymore) and print the status message in case it is not finished
  
  terminationStatus <- quite_iteration()
    
    if (!terminationStatus$timeToQuit) { 
    wrapUp_day() 

    } else {
    cat(terminationStatus$message, "\n")
    }
     
} # end while (!terminationStatus$timeToQuit)

cat("----------------------------------------------------------------------------\n
     Results are saved in ",path,"\n")
       
if (verbose) cat("dayLoop() : END\n")
} # end dayLoop()
