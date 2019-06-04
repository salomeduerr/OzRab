#
# name: OzRab
# 
# This is the main function of the model
# 
# called by the runScript
#

OzRab <- function (OzRabArgs = OzRabArguments(), sourcingOK=TRUE) {       

# check if sourcing was OK
if (!sourcingOK) stop("ERROR in OzRab() : It seems like sourcing failed. Quitting!")

initialize_arguments(OzRabArgs)
 
# In case seed was set to 0 (i.e. the default values of kept) initialize_Arguments selected a
# random seed. This random seed has to be put in the OzRabArgs list for reporting purposes.
OzRabArgs$seed <- seed

# reads the input data, establish the allDog data.frame(i.e. all dogs collated together for the non-rabies situation),
# defines the index dog(s) and updates the allDogs table, sets simTIME = 0, creates results folder

initialize_simulation()
#initialize_simulation_communities()    # used for Emily's work Oct 2017
#initialize_simulation_categories()     # used for Emily's work Oct 2017

# Save all the used options and their values to a file (klunky but it works) and as Rdata
sink(file=paste(path,"options.txt",sep="/"))
  print(OzRabArgs)
sink()
save(OzRabArgs, file=paste(path,"OzRabArguments.Rdata",sep="/"))

# pre-emptive vaccination of the dog population in the affected region
# either on the household level (all dogs of the households will be vaccinated) or on
# the individual dog level (x% of all dogs are vaccinated irrespectively of living in
# the same or different households)
affected_region <- allDogs[which(allDogs$index_dog==T), "region"]
if (preemptive_vacc == TRUE & preemptVacc_covLevel_reference == "household") preemptive_vaccination_hh(affected_region)
if (preemptive_vacc == TRUE & preemptVacc_covLevel_reference == "dog") preemptive_vaccination_dog(affected_region)

# The dayLoop() function loops though the days. This is basically a while loop running as long
# as simTIME has'nt reach the defined maximal simulation time
dayLoop()

wrapUp_simulation()

cat("SIMULATION TERMINATED OK\n") # This line is important for the script that make the daily
                                  # builds of the trunk (aka bleeding edge release). Do not remove, move, 
                                  # or alter this line in any way. The building script (build.sh) performs 
                                  # some basic sanity checks of the code and is looking for this line to
                                  # establish if the simulation crashed or managed to finish properly.    

} # end OzRab()
