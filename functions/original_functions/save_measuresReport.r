#
# name: save_measuresReport()
#
# This file calculates and summarize some characteristics of the current run into one file:
# - outbreak duration (end of the outbreak is defined as the date the last dog died)
# - outbreak size: defined as the number of dogs died from rabies
# -	number of moved dogs during the outbreak (i.e. those dogs transmitted the disease from one 
#   community to the other)
# -	number of vaccinated dogs
# - mean Rt (effective reproductive ratio) over the entire outbreak
# - Vaccination coverage (for pre-emptive and reactive) on animal level in case the vaccination 
#   coverage in the model is based on the household level
#
# called by: standardSummary(final)
#

save_measuresReport <- function() {

if (verbose) cat("save_measuresReport() : START\n")

# define the outbreak duration based on the latest mortality date of all dogs
outbreak_dur <- max(allDogs$mortalityDate[allDogs$dead==1])

# define outbreak size as the number of dogs died in total
nb_dead_dogs <- sum(allDogs$dead)

# report the number of dogs rabid
nb_rabid_dogs <- sum(allDogs$infectious)

# report the total number of moved dogs
nb_moved_dogs <- sum(allDogs$moved)

# report the number of moved dogs that lead to an infection in the new community
# this is the same as the total number of moved dogs but for the vaccination scenarios
# where it can be that a moved vaccinated dog won't develop rabies in the new community
nb_infect_moved_dogs <- length(which(allDogs$moved ==1 & allDogs$infectious==1))

# report the total number of vaccinated dogs
nb_vacc_dogs <- length(which(allDogs$vaccinated==1))

# report the total number of culled dogs
nb_cull_dogs <- length(which(allDogs$cullDetectDogs==1)) +
                length(which(allDogs$cullContactedDog==1)) + 
                length(which(allDogs$cullReactive==1))
detectDogs_culled <- detectDogs_culled
contDogs_culled <- dogs_contactCulled
nb_reactiveCull <- dogs_reactiveCulled
   

# dog level vaccination coverage of the pre-emptive vacciantion in case the pre-emptive
# vaccination has been executed on the household level (i.e. a given percentage of 
# household, not dogs, has been vaccinated)
if (preemptive_vacc == TRUE & preemptVacc_covLevel_reference == "household") { 

 preemptiveVacc_cov_dogLevel <- round(percentage_preemptivelyVaccDogs, digits= 3)
 
 } else { preemptiveVacc_cov_dogLevel <- NA }
 
# dog level vaccination coverage of the reactive vacciantion in case the reactive
# vaccination has been executed on the household level (i.e. a given percentage of 
# household, not dogs, has been vaccinated)
if (react_vaccination == TRUE & reactVacc_covLevel_reference == "household") {

 if (vaccination_goal == "community") {

  affected_comms <- unique(allDogs$community[allDogs$dead == 1])
  nb_dogs_affectedComms <- length(which(allDogs$community %in% affected_comms))
  nb_vacc_dogs <- length(which(allDogs$vaccinated == 1))
  reactiveVacc_cov_dogLevel <- round(nb_vacc_dogs / nb_dogs_affectedComms, digits= 3)
 
 } else {
 
  affected_region <- allDogs$region[allDogs$index_dog == T]
  nb_dogs_affectedRegion <- length(which(allDogs$region == affected_region))
  nb_vacc_dogs <- length(which(allDogs$vaccinated == 1))
  reactiveVacc_cov_dogLevel <- round(nb_vacc_dogs / nb_dogs_affectedRegion, digits= 3)
 }  
  
  } else { reactiveVacc_cov_dogLevel <- NA }


# report the Rt (effective reproduction number) as mean over the entire outbreak
# is expected to be 1
Rt_mean <- mean(allDogs$nb_secCases[allDogs$infectious==1])


# collate all measures and save it in the results folder
measuresReport <- cbind(duration = outbreak_dur,
                        tot_deaths = nb_dead_dogs,
                        rabid_dogs = nb_rabid_dogs,
                        moved = nb_moved_dogs, 
                        mov_DK = betwComm_kernelMovs_counter,
                        movInfect = nb_infect_moved_dogs,
                        n_vacc = nb_vacc_dogs,
                        n_cull = nb_cull_dogs,
                        detDogs_cull = detectDogs_culled,
                        contDogs_cull = contDogs_culled,
                        reactCull = nb_reactiveCull, 
                        preemptVaccCov_dogLevel = preemptiveVacc_cov_dogLevel,
                        reactVaccCov_dogLevel = reactiveVacc_cov_dogLevel,
                        mean_Rt = round(Rt_mean, digits=3))

fileName <- paste(path,"measuresReport.txt",sep="/")
write.table(measuresReport, fileName, row.names=FALSE, quote=FALSE, sep="\t")                

if (verbose) cat("save_measuresReport() : END\n")

} # end save_measuresReport()

