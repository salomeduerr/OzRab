##
# The run script can be copy - paste into the R console.
# The path indicated in the setwd() is where all the following folders are located:
# - functions: containing all the functions
# - scenario: containing the input files (incl Dog tables) per scenarios
# - results: where the results from each scenario will be stored
# - library: containing required packages
## 

setwd("YOUR_DIRECTORY") #change to your own directory



# First: The functions and required libraries will be sources via source_FunctionsLibraries() 
# if the sourcing was OK the function returns (sourcingOK = T) else (sourcingOK = F)
source("functions/source_FunctionsLibraries.r")
library(sp)
library(stringr)
library(plyr)
library(mvtnorm)
sourcingOK <- source_FunctionsLibraries()

population <- c(rep("EH",27), rep("RH",27), rep("SH",27), rep("FP",27))
S <- c(rep(0.2,27),rep(0.2,27),rep(0.6,27),rep(0.42,27))
R <- c(rep(0.2,27),rep(0.6,27),rep(0.2,27),rep(0.29,27))
E <- c(rep(0.6,27),rep(0.2,27),rep(0.2,27),rep(0.29,27))

scenario <- 1:length(population)


covac<-as.data.frame(expand.grid( c(0.5,0.7,0.9),c(0.5,0.7,0.9),c(0.5,0.7,0.9)))
VaccE <- rep(covac[,3],4)
VaccR <- rep(covac[,2],4)
VaccS <- rep(covac[,1],4)


pop.1 <- data.frame(S,R,E,scenario,VaccE, VaccR, VaccS,population)
scenario <- paste(pop.1$population,"_E",pop.1$VaccE,"_R",pop.1$VaccR,"_S",pop.1$VaccS, sep = "")
pop <- data.frame(S,R,E,scenario,VaccE, VaccR, VaccS,population)



# Secons: Run the model via the main function OzRab().
# This function has two arguments:
# 1. A list of all argument (or parameter) values. Their default values are defind in OzRabArguments(),
#    here the user is able to overwrite these default values by including the values in the list below.
# 2. sourcingOK: TRUE or FALSE (defined by the source_FunctionsLibraries() above), if FALSE, the model
#    will not run and print this to the console
OzRab(OzRabArguments(

 # List of argument values to overwrite the default values in OzRabArguments()
 #seed = 0,                                      # Seed for random number generator. Any integer number different from zero (0)
                                                 # is used directly as the seed, if seed is set to zero the seed is set radomly by R
                                                 # within the function initialize_arguments (i.e. a different seed is used everytime)
 verbose=T,                                      # if TRUE the model is much more narrative (not used for normal model applications)
 max_time = 10000,                               # number of days the model will not exceed for this outbreak
 scenarioDataPath = "GitHub_anonymized",         # name of the scenario folder within YOUR_DIRECTORY/scenario
 kernel_type = "category",     
 results_folder =  "Vacc_Test",                  # name of the results folder 
 save_categories_contacts_YN = T,
 nb_index_dogs = 1,                              # number of index dogs
 index_region = "NPA",                           # index region: HAS to be defined unless more detailed information are given via index_comm, index_hh or index_dog
 index_community = "A",                          # index community(ies): can be defined
# index_hh = "",                                 # index household(s): can be defined
# index_dog = "",                                # index dog(s): can be defined

 # control strategy options can be switched on (T) or off (F)
 react_vaccination = T,
 react_vacc_cov_explorer = 0.9,
 react_vacc_cov_roamer = 0.9,
 react_vacc_cov_stayhome = 0.9,

 reactVacc_covLevel_reference = "dog",
 preemptive_vacc = F,
 displayEpiMap = F,
 preemptVacc_covLevel_reference = "dog",
 vaccination_goal= "region",
 cull_detectDogs = T,
 reactive_cull = F,
 cull_contactDogs = F,
 culling_goal = "community",
 consider_vaccination = "yes",
 movementBan_BC = F,
 compliance_BCMovs = 0.9,
 movementBan_BHH = F,
 compliance_BHHMovs = 0.9,
 start_BHHban_delay = 2

 ), sourcingOK)
 
