#                       
# name: OzRabArguments
#
# This function is an argument of the main function OzRab
# - list of default arguments (all time periods in days)
# - overwrites the defaults with the values from the runsScript
# - returns the defaults
#
# called by OzRab (as argument)
#


OzRabArguments <- function(...){

# First: read in the user defind argument values  
user_args <- list(...)

# Second: read in the default argument values
defaults <- list(

 ## general arguments 
  # Seed for random number generator. Any integer number different from zero (0)
  # is used directly as the seed, if seed is set to zero the seed is set radomly by R
  # within the function initialize_arguments (i.e. a different seed is used everytime)
  seed = 0,
  # whether or not the model should print information into the console while running
  verbose= FALSE,  
  # name of the subfolder in the folder scenario  
  scenarioDataPath = NULL,
  # time how long the model should run
  max_time = 365, 
  # kernel_type: .....
  kernel_type = "overall",
 
 ## index dog(s) related parameters
 # definition of the case(es) is hierarchical: if index_dog is defined, all other are useless (not used in the model);
 # else if index_hh is defined, index_community and index_region are useless, else if index_community is defined, 
 # index_region is not used in the model
  index_region    = NA,
  index_community = NA,
  index_hh        = NA,
  index_dog       = NA,
  nb_index_dogs   = 1,
  
 ## disease states related parameters
  # time period between exposure (bite) and infectiouness (incubation period)
  infectiousDelay = c(infecDelayDistr="pert", infecDelayParam=c(22.8,25.8,29)),
  # time period between infectiousness and occurrence of clinical signs
  clinicalDelay =  c(clincDelayDistr="unif", clinicDelayParam=c(1,3)),
  # time period between start of clinical signs and death
  mortalityDelay = c(mortDelayDistr="pert", mortDelayParam=c(2,4.7,12)),
  
 ## within household contact/transmission related arguments
  # daily probability of having a contact between two dogs living in the same household 
  # when the dog is infectious but before is shows any clinical signs
  cont_prob_sameHH = c(contSameHHDistr="unif", contSameHHParam=c(0.94,1)),
  # probability of bite given a contact within the same household
  # when the dog is infectious but before is shows any clinical signs
  bite_prob_sameHH = c(biteSameHHDistr="unif", biteSameHHParam=c(0.01,0.05)), 
  # increase of the bite probability when the dog is becoming rabid (showing clinical signs)
  increasedBiteProb_sameHH = 10,
  # probability of rabies transmission given a bite (valuable for all type of contact)
  transmissionProb = c(clincDelayDistr="pert", clinicDelayParam=c(0.45,0.49,0.52)),
  
 ## between household contact/transmission related arguments
  # probability of bite given a contact between dogs from different households
  bite_prob_betweenHH = c(biteBetwHHDistr="unif", biteBetwHHParam=c(0.1,0.3)), 
  # increase of the bite probability when the dog is becoming rabid (showing clinical signs)
  increasedBiteProb_betwHH = 3,
 
 ## between community short term movement parameters
 # period of how long the dog stays in the visited community before moving back 
 # to the original community
  visit_period = c(visit_periodDistr="unif", visit_periodParam=c(1,2)),
  
 ## detection of rabies: time in days for detection of rabies after the start of 
  # clinical signs for the first case (detectPeriod_firstCase) and all other cases 
  # in the region (detectPeriod_secondCases)
  detectPeriod_firstCase= c(detectPeriodDistr_first="pert", detectPeriodParam_first=c(14,21,28)),
  detectPeriod_secondCases= c(detectPeriodDistr_second="pert", detectPeriodParam_second=c(1,2,4)),
 
 ## control strategy vaccination during the outbreak (reactive vaccination)
  # switch T/F defining whether reactive vaccination is applied as control strategy
  react_vaccination = FALSE, 
  # definition of the vaccination coverage can be based on the dog population (i.e. a
  # given percentage of dogs will be vaccinated irrespectively of whether they are living in
  # the same or different household; "dog") or on the households level (i.e.
  # a given percentage of households will be vaccinated and all dogs in that hh are 
  # vaccinated; "household")
  reactVacc_covLevel_reference = "household",    # alternative = "dog"
  # vaccination goal is either "community" or "region", indicating whether the
  # goal of the vaccination strategy is to vaccinate all dogs in the community or region 
  # where rabies is detected
  vaccination_goal= "region",
  # time period between the detection of the first rabid dog and the start of 
  # the vaccination campaign
  start_vacc_delay= 7,
  # number of dogs maximally vaccinated per day
  vacc_capacity= 50, 
  # reached immunization coverage in the community or region by reactive vaccination
  reactive_vacc_cov= 0.7,
  ### These are for the vaccination levels for each category they have to average 70%  Emily OCT 2017
  react_vacc_cov_explorer = 0.7,
  react_vacc_cov_roamer = 0.7,
  react_vacc_cov_stayhome = 0.7,
 
  # delay between vaccination of the dog and the protection of the dog through
  # vaccination. There is no protection through vaccination during the time before
  # protection has been reached.
  protectionDelay = c(protDelayDistr="unif", protDelayParam=c(7,14)), 
  # efficacy of the vaccination if the vaccination is done less than last_vacc day
  # after the exposure of the dog to rabies
  vaccEfficacy = c(vaccEfficacyDistr="unif", vaccEfficacyParam=c(0.92,0.96)),
  # efficacy of the vaccination if the vaccination is done more than last_vacc day
  # after the exposure of the dog to rabies
  reduced_vaccEfficacy =  c(red_vaccEfficacyDistr="unif", red_vaccEfficacyParam=c(0.05,0.25)),
  # number of days determines the change of the vaccination efficacy (from vaccEfficacy to
  # reduced_vaccEfficacy) 
  late_vacc= c(late_vaccDistr="unif", late_vaccParam=c(2,4)),

 ## control strategy pre-emptive vaccination
  # switch T/F defining whether the pre-emptive vaccination should be executed 
  preemptive_vacc = FALSE,
  # definition of the vaccination coverage can be based on the dog population (i.e. a
  # given percentage of dogs will be vaccinated irrespectively of whether they are living in
  # the same or different household; "dog") or on the households level (i.e.
  # a given percentage of households will be vaccinated and all dogs in that hh are 
  # vaccinated; "household") 
  preemptVacc_covLevel_reference = "household",  # alternative = "dog"
  # reached immunization coverage in the region for the pre-emptive vaccination
  preemptive_vacc_cov = 0.7,
 
 ### These are for the vaccination levels for each category they have to average 70%  Emily OCT 2017
 preemptive_vacc_cov_explorer = 0.9,
 preemptive_vacc_cov_roamer = 0.9,
 preemptive_vacc_cov_stayhome = 0.3,
  
 ## control strategy culling
  # switch T/F whether or not detected dogs will be culled/removed from the population
  # immediately (i.e. at the day of detection)
  cull_detectDogs = FALSE,
  # time period between the detection of the first rabid dog and the start of 
  # the immediate culling of rabid dogs campaign
  start_cullDetectDog_delay = 1,
  # switch T/F whether culling of dog contacted by a detected rabid dog is applied 
  # as a control strategy
  cull_contactDogs = FALSE,
  # time period between the detection of the first rabid dog and the start of 
  # culling of contacted dogs
  start_cullContactedDog_delay = 4,
  # proportion of the dogs contacted by detected rabid dogs that will be culled 
  cullProp_contactedDogs = 0.8,
  # switch T/F whether reactive culling is applied as a control strategy
  reactive_cull = FALSE,
  # time period between the detection of the first rabid dog and the start of 
  # the reactive culling campaign
  start_reactiveCull_delay = 7,
  # culling goal is either "community" or "region" so far, indicating whether the
  # goal of the culling strategy is to cull all dogs in the community or region 
  # where rabies is detected
  culling_goal= "community",
  # defines whether or not the vaccination status of the dogs should be considered when
  # culling is applied. If set to "yes", no vaccinated dog will be culled, if set to "no",
  # the vaccination status will not be taken into account during culling 
  consider_vaccination = "yes",
  # proportion of the population that will be culled in the reactive culling control option
  reactiveCull_prop = 0.5,
  # number of dogs maximally culled per day
  cull_capacity= 15, 
   
 ## control strategy movement restrictions
  # switch T/F defining whether the control strategy "total movement ban between communities"
  # is executed
  movementBan_BC = FALSE,
  # dog owners compliance of the movement ban between communities
  compliance_BCMovs = 0.8,
  # time period between the detection of the first rabies case and the start of the movement bans
  # between communities
  start_BCban_delay = 4,
  # switch T/F defining whether the control strategy "Keep dogs inside the yards" is executed
  movementBan_BHH = FALSE,
  # dog owners compliance of the movement ban between households
  compliance_BHHMovs = 0.6,
  # time period between the detection of the first rabies case and the start of the movement bans
  # between households
  start_BHHban_delay = 4,
  
 ## result saving parameters:
  # folder name of the first result subfolder, can also have subfolders as e.g ("test/no_vacc")
  results_folder = NULL, 
  # should the epimap be showed on the screen
  displayEpiMap = TRUE,
  # should the epi maps be saved as PDF and / or bitmap?
  saveEpiMapPDF  = TRUE,
  saveEpiMapBitmap =TRUE,
  # should the daily status messages be saved into the results folder?
  saveStatusMessage = TRUE,
  # Display status message in console (T) or not (F)
  displayStatusMessage = TRUE,
  # should the daily contacts be recorded?
  save_contacts_YN = TRUE,
  # should the daily movements be recorded?
  save_movements_YN =TRUE,
  # should a final list (sum up over the entire iteration) of dogs exposed to rabies be recorded?
  save_rabidDogs_YN = TRUE,
  # should a final list (sum up over the entire iteration) of dogs vaccinated against rabies be recorded?
  save_vaccDogs_YN = TRUE,
  # should a final list (sum up over the entire iteration) of culled dogs be recorded?
  save_cullDogs_YN = TRUE,
  # should a final list (sum up over the entire iteration) of dogs moved be recorded?
  save_movedDogs_YN = TRUE,
  # should information in the index dog(s) be recorded?
  save_indexDog_YN = TRUE,
  # should a report containing outbreak duration, outbreak size, nb of dogs moved, 
  # vaccinated and culled and R0 be saved?
  save_measuresReport_YN = TRUE,
  # should the stats and the graph showing secondary cases of rabid dogs be saved?
  save_secondaryCases_YN = TRUE,
  # should a epidemic curve be saved for the current iteration?
  save_epiCurves_YN = TRUE,
  # should the parameter values be analysed and saved together with outbreak measures?
  save_paramValues_YN = TRUE,
  # if you want a column of how many contacts from each category (onlt T if in category
 # mode not community or overall)
 save_categories_contacts_YN = FALSE,
 
 
 ## final arguments
  # Should the globally defined variables be cleared when the simulation
  # terminates? Under normal conditions (i.e. non-debug mode) it is wise to clean
  # up after the simulation. 
  rmGlobalVarsOnTermination = TRUE   
)   # end default list


# Third: overwrite the default argument values by the user defines argument values
if (length(user_args)>0){

  # Check which values to change
  changes <- match(names(user_args),names(defaults))
  cat(length(changes)," default options are overridden\n")

  # Stop if trying to change nonexisting parameter
  if (any(is.na(changes)))
    stop("ERROR in OzRabArguments() :", paste( paste(names(user_args)[is.na(changes)],collapse=", "), " is/are not valid parameter names.\n."))

  # Change the requested values and return
  for (i in 1:length(changes)){
    defaults[[changes[i]]] <- user_args[[i]]
  }
} # end if (length(user_args))


return(defaults)
} # end OzRabArguments