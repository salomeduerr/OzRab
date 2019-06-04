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

### These are for the vaccination levels for each category they have to average 70%  Emily OCT 2017 if kernel type NOT category then original
## is used (else statment below)

if (kernel_type == "category") {
dogPop_affectedRegion_stayhome <- allDogs[allDogs$region == region & allDogs$index_dog == F & allDogs$category == "Stayhome",]
dogPop_affectedRegion_roamer <- allDogs[allDogs$region == region & allDogs$index_dog == F & allDogs$category == "Roamer",]
dogPop_affectedRegion_explorer <- allDogs[allDogs$region == region & allDogs$index_dog == F & allDogs$category == "Explorer",]

nb_dogs_vaccintated_stayhome <- round(nrow(dogPop_affectedRegion_stayhome) * preemptive_vacc_cov_stayhome)
nb_dogs_vaccintated_roamer <- round(nrow(dogPop_affectedRegion_roamer) * preemptive_vacc_cov_roamer)
nb_dogs_vaccintated_explorer <- round(nrow(dogPop_affectedRegion_explorer) * preemptive_vacc_cov_explorer)

vacc_dogs_stayhome <- sample(dogPop_affectedRegion_stayhome[,1], nb_dogs_vaccintated_stayhome)
vacc_dogs_roamer <- sample(dogPop_affectedRegion_roamer[,1], nb_dogs_vaccintated_roamer)
vacc_dogs_explorer <- sample(dogPop_affectedRegion_explorer[,1], nb_dogs_vaccintated_explorer)

vacc_dogs <- c(vacc_dogs_stayhome, vacc_dogs_roamer, vacc_dogs_explorer)

} else {
  nb_dogs_vaccintated <- round(nrow(dogPop_affectedRegion) * preemptive_vacc_cov)
  vacc_dogs <- sample(dogPop_affectedRegion[,1], nb_dogs_vaccintated)
}

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