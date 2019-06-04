#
# name: preemptive_vaccination_dog
#
# Pre-emptive vaccination is executed before the dayLoop and selects user defined
# percentage of the dogs in the region of the index dog to be vaccinated and protected
# before the start of the outbreak. The susceptibility of these dogs is reduced
# according to the vaccine efficacy parameter.
#
# called by OzRab()
#


preemptive_vaccination_dog <- function(region) {

if (verbose) cat("preemptive_vaccination_dog() : START\n")

# define the dog population in the affected region
dogPop_affectedRegion <- allDogs[allDogs$region == region & allDogs$index_dog == F,]

# select a given percentage of these dogs as vaccinated
nb_dogs_vaccintated <- round(nrow(dogPop_affectedRegion) * preemptive_vacc_cov)
vacc_dogs <- sample(dogPop_affectedRegion[,1], nb_dogs_vaccintated)

# set the vaccination and protection status and dates
allDogs[vacc_dogs,"vaccinated"] <<- 1
allDogs[vacc_dogs,"vaccinationDate"] <<- 0
allDogs[vacc_dogs,"protected"] <<- 1
allDogs[vacc_dogs,"protDateScheduled"] <<- 0
allDogs[vacc_dogs,"protectionDate"] <<- 0

# set the susceptibility value of the vaccinated dogs
vaccEfficacyDistr <- eval(parse(text=paste("r",vaccEfficacy [[1]],sep="")))
vaccEff <- vaccEfficacyDistr(nb_dogs_vaccintated,
                             as.numeric(vaccEfficacy [[2]]),
                             as.numeric(vaccEfficacy [[3]]))

allDogs$susceptibility[vacc_dogs] <<- 1 - vaccEff

if (verbose) cat("preemptive_vaccination_dog() : START\n")

} # end preemptive_vaccination_dog()