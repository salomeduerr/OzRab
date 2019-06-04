##
# The run script can be copy - paste into the R console.
# The path indicated in the setwd() is where all the following folders are located:
# - functions: containing all the functions
# - scenario: containing the input files (incl Dog tables) per scenarios
# - results: where the results from each scenario will be stored
# - library: containing required packages
## 


setwd("G:/VPHI/Epi/Projects/66_RabiesModelling_USyd (Dürr)/OzRab")

# First: The functions and required libraries will be sources via source_FunctionsLibraries() 
# if the sourcing was OK the function returns (sourcingOK = T) else (sourcingOK = F)
source("functions/source_FunctionsLibraries.r")
sourcingOK <- source_FunctionsLibraries()

# Secons: Run the model via the main function OzRab().
# This function has to arguments:
# 1. A list of all argument (or parameter) values. Their default values are defind in OzRabArguments(),
#    here the user is able to overwrite these default values by including the values in the list below.
# 2. sourcingOK: TRUE or FALSE (defined by the source_FunctionsLibraries() above), if FALSE, the model
#    will not run and print this to the console
OzRab(OzRabArguments(

 # List of argument values to overwrite the default values in OzRabArguments()
 seed = 10,                                       # Seed for random number generator. Any integer number different from zero (0)
                                                 # is used directly as the seed, if seed is set to zero the seed is set radomly by R
                                                 # within the function initialize_arguments (i.e. a different seed is used everytime)
 verbose=T,                                      # if TRUE the model is much more narrative (not used for normal model applications)
 max_time = 1000,                                # number of days the model will not exceed for this outbreak
 scenarioDataPath = "censusNPA_GaliRandom",      # name of the subfolder in the folder scenario
 results_folder =  "default_results",            # name of the results folder 
 nb_index_dogs = 1,                              # number of index dogs
 index_region = "NPA",                           # index region: HAS to be defined unless more detailed information are given via index_comm, index_hh or index_dog
 index_community = c("Bamaga","Seisa"),                    # index community(ies): can be defined
# index_hh = "Ba1",                              # index household(s): can be defined
 index_dog = "Se15",                            # index dog(s): can be defined

 # control strategy options can be switched on (T) or off (F)
 react_vaccination = F,
 reactVacc_covLevel_reference = "household",
 preemptive_vacc = F,
# preemptVacc_covLevel_reference = "dog",
 vaccination_goal= "region",
 cull_detectDogs = F,
 reactive_cull = F,
 cull_contactDogs = F,
 culling_goal = "community",
 consider_vaccination = "yes",
 movementBan_BC = F,
 compliance_BCMovs = 0.9,
 movementBan_BHH = F,
 compliance_BHHMovs = 0.9,
 start_BHHban_delay = 2

 # testing some parameter values
# clinicalDelay =  c(clincDelayDistr="unif", clinicDelayParam=c(0,0)),
# bite_prob_sameHH = c(biteSameHHDistr="unif", biteSameHHParam=c(0.2,0.3)),
# bite_prob_betweenHH = c(biteBetwHHDistr="unif", biteBetwHHParam=c(0.2,0.3)),
# detectPeriod_secondCases= c(detectPeriodDistr_second="pert", detectPeriodParam_second=c(0,0,0))

 ), sourcingOK)
 