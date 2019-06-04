#
# name: preemptive_vaccination_hh
#
# Pre-emptive vaccination is executed before the dayLoop and selects user defined
# percentage of households in the region of the index dog to be vaccinated and protected
# before the start of the outbreak.
# All dogs in these households will be vaccinated and its susceptibility is reduced
# according to the vaccine efficacy parameter.
#
# called by OzRab()
#


preemptive_vaccination_hh <- function(region) {

if (verbose) cat("preemptive_vaccination_hh() : START\n")

# define the dog population in the affected region, the number of household in this region
dogPop_affectedRegion <- allDogs[allDogs$region == region,]
nh_hh_affectedRegion <- length(unique(dogPop_affectedRegion$hh_ID))

# select a given percentage of the households in this region as vaccinated
nb_hh_vaccintated <- round(nh_hh_affectedRegion * preemptive_vacc_cov)
vacc_hh <- sample(unique(dogPop_affectedRegion$hh_ID), nb_hh_vaccintated)

# define the dogs in the vaccinated households and the dog-based vaccination coverage
# which will become a global variable to be recorded
vacc_dogs <- which(allDogs$hh_ID %in% vacc_hh & allDogs$index_dog == F)
percentage_preemptivelyVaccDogs <<- length(vacc_dogs) / nrow(dogPop_affectedRegion)

cat("percetage of vaccinated households is: ", nb_hh_vaccintated / nh_hh_affectedRegion,
    "\n percentage of vaccinated dogs is:", percentage_preemptivelyVaccDogs, "\n")

# set the vaccination and protection status and dates
allDogs[vacc_dogs,"vaccinated"] <<- 1
allDogs[vacc_dogs,"vaccinationDate"] <<- 0
allDogs[vacc_dogs,"protected"] <<- 1
allDogs[vacc_dogs,"protDateScheduled"] <<- 0
allDogs[vacc_dogs,"protectionDate"] <<- 0

# set the susceptibility value of the vaccinated dogs
vaccEfficacyDistr <- eval(parse(text=paste("r",vaccEfficacy [[1]],sep="")))
vaccEff <- vaccEfficacyDistr(length(vacc_dogs),
                             as.numeric(vaccEfficacy [[2]]),
                             as.numeric(vaccEfficacy [[3]]))

allDogs$susceptibility[vacc_dogs] <<- 1 - vaccEff

if (verbose) cat("preemptive_vaccination_hh() : START\n")

} # end preemptive_vaccination_hh()