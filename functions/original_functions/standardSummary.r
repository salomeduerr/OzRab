#
# name: standardSummary
#
# Args:
#   step : indicates what type of summary is supposed to be performed, valid
#          values are:
#          'init' : initial summaries,
#          'day'  : summaries to perform at the end of each time unit (i.e.
#                   day),
#          'final': summaries to perform at the end of the simulation
# 
# called by dayLoop() summaryFunction("init") 
#           dayLoop() summaryFunction("day") 
#           wrapUp_simulation() summaryFunction("final")
#

standardSummary <- function(step){
  switch(step,

         ###############
         #             #
         #    init     #
         #             #
         ###############
         init={if (verbose) cat("summaries$init() : START\n")

           # dailyNumberReport file: build up and save in the results folder
           fileName = paste(path,"dailyNumberReport.txt",sep="/")
  
           # define the susceptible population at the start of the outbreak in the region of the
           # index dog(s) as the number of dogs in the region - number of dogs dead dogs - number
           # of exposed (including index dogs) - number of vaccinated dogs considering their 
           # susceptibility status
           
           # number dead dogs
           n_death <- length(which(allDogs$dead == 1))
          
           # number of exposed (but not yet dead) dogs
           n_exposed <- length(which(allDogs$exposed == 1 & allDogs$dead == 0))
           
           # number of dogs with protective vaccination but neither dead nor exposed
           n_vacc_protection <- length(which(allDogs$vaccinated == 1 & allDogs$exposed != 1 & allDogs$dead == 0)) -
                                sum(allDogs$susceptibility[allDogs$vaccinated == 1 & allDogs$exposed != 1 & allDogs$dead == 0])
           
           # sum up the number of susceptible dogs at the start of the outbreak
           start_pop_region <- length(which(allDogs$region %in% allDogs$region[allDogs$index_dog])) -
                               n_death - n_exposed - n_vacc_protection  
  
           initialCensus <- data.frame(Day     = 0,
                                       Dogs_cont = sum(allDogs$index_dog),
                                       HH_cont = length(unique(allDogs$hh_ID[allDogs$index_dog==T])),
                                       Dogs_exp = sum(allDogs$index_dog),
                                       HH_exp = length(unique(allDogs$hh_ID[allDogs$index_dog==T])),
                                       Dogs_inf = 0,
                                       HH_inf = 0,
                                       Dog_clin = 0,
                                       HH_clin = 0,
                                       Dogs_dead = 0,
                                       HH_dead = 0,
                                       Dogs_vacc = length(which(allDogs$vaccinated == 1)),
                                       HH_vacc = length(unique(allDogs$hh_ID[allDogs$vaccinated == 1])),
                                       DetDog_cull = 0,
                                       HH_DetDogCull = 0,
                                       ContDog_cull = 0,
                                       HH_ContDogCull = 0,
                                       React_cull = 0,
                                       HH_reactCull = 0,
                                       WHHC = 0,
                                       WHHT = 0,
                                       BHHC = 0,
                                       BHHT = 0,
                                       BCC = 0,                                       
                                       BCT = 0,
                                       movements = 0,
                                       susceptPop_region = start_pop_region)

           write.table(initialCensus, fileName, append=FALSE, row.names=FALSE, quote=FALSE, sep="\t")                 

           # Reset the graphical windows
           graphics.off()

           # Open and set up graphics windows for the plots that have been
           # enabled. Since we are calling 'updateCharts' right after this
           # initial device setup nothing has to be plotted here.
           if (displayEpiMap) {
             x11(height=8, width=8) # Open initial epi map window
             deviceNumberEpiMap <<- dev.cur() # Record device number of epi map window
             plot(0,0,col="white")
             title("Epi map pending...")
           }
         
         if (verbose) cat("summaries$init() : END\n")
         }, # end init{}

                          
         ###############
         #             #
         #    day      #
         #             #
         ###############
         day={

         if (verbose) cat("summaries$day() : START\n")

         # define the number of new contacts, exposures, infectious, clinical and dead dogs
         contacts_today <- which(allDogs$contactDate == simTIME)
         exposure_today <- which(allDogs$contactDate == simTIME & allDogs$exposed == 1)
         infectious_today <- which(allDogs$infectiousDate == simTIME)
         clinical_today <- which(allDogs$clinicalDate == simTIME)
         dead_today <- which(allDogs$mortalityDate == simTIME)
         vacc_today <- which(allDogs$vaccinationDate == simTIME)
         detecDogsCulled_today <- which(allDogs$mortalityDate == simTIME & allDogs$cullDetectDogs == 1)
         contDogsCulled_today <- which(allDogs$mortalityDate == simTIME & allDogs$cullContactedDog == 1)
         reactCulled_today <- which(allDogs$mortalityDate == simTIME & allDogs$cullReactive == 1)
         movements_today <- which(allDogs$movementDate == simTIME)

         # define the susceptible population today in the region of the index dog(s)
         # as the number of dogs in the region - number of dogs dead dogs - number
         # of exposed dogs - number of vaccinated dogs considering their susceptibility status
           
         # number dead dogs
         n_death <- length(which(allDogs$dead == 1))
         
         # number of exposed (but not yet dead) dogs
         n_exposed <- length(which(allDogs$exposed == 1 & allDogs$dead == 0))
           
         # number of dogs with protective vaccination but neither dead nor exposed
         n_vacc_protection <- length(which(allDogs$vaccinated == 1 & allDogs$exposed != 1 & allDogs$dead == 0)) -
                                sum(allDogs$susceptibility[allDogs$vaccinated == 1 & allDogs$exposed != 1 & allDogs$dead == 0])
           
         # sum up the number of susceptible dogs today
         susceptPop_today <- length(which(allDogs$region %in% allDogs$region[allDogs$index_dog])) -
                             n_death - n_exposed - n_vacc_protection
         
         
           ##################################################
           #  Display censused data on the console          #
           ##################################################
           cat("New cases of anmials: contacted: ", length(contacts_today),
               ", exposed: ", length(exposure_today),
               ", infec: ", length(infectious_today),
               ", clin: ", length(clinical_today),
               ", dead: ", length(dead_today),
               ", vaccinated: ", length(vacc_today),
               ", detect culled: ", length(detecDogsCulled_today),
               ", contact culled: ", length(contDogsCulled_today),
               ", reactively culled: ", length(reactCulled_today),          
               ", moved: ", length(movements_today),
               "\n",sep="")
 
           ##################################################
           #    Update charts                               #
           ##################################################
           
           # define infected dogs:
           infected_dogs_lines <- which(allDogs$contact !=0)
           infected_dogs <- allDogs[infected_dogs_lines,]
           
           update_charts(infected_dogs) 
         
         
           ##################################################
           #    Update dailyNumberReport                    #
           ##################################################

           currentCensus <- c(Day     = simTIME,
                                       Dogs_cont = length(contacts_today),
                                       HH_cont = length(unique(allDogs$hh_ID[contacts_today])),
                                       Dogs_exp = length(exposure_today),
                                       HH_exp = length(unique(allDogs$hh_ID[exposure_today])),
                                       Dogs_inf = length(infectious_today),
                                       HH_inf = length(unique(allDogs$hh_ID[infectious_today])),
                                       Dog_clin = length(clinical_today),
                                       HH_clin = length(unique(allDogs$hh_ID[clinical_today])),
                                       Dogs_dead = length(dead_today),
                                       HH_dead = length(unique(allDogs$hh_ID[dead_today])),
                                       Dogs_vacc = length(vacc_today),
                                       HH_vacc = length(unique(allDogs$hh_ID[vacc_today])),
                                       DetDog_cull = length(detecDogsCulled_today),
                                       HH_DetDogCull = length(unique(allDogs$hh_ID[detecDogsCulled_today])),
                                       ContDog_cull = length(contDogsCulled_today),
                                       HH_ContDogCull = length(unique(allDogs$hh_ID[contDogsCulled_today])),
                                       React_cull = length(reactCulled_today),
                                       HH_reactCull = length(unique(allDogs$hh_ID[reactCulled_today])),
                                       WHHC = nb_WHHC,
                                       WHHT = nb_WHHT,
                                       BHHC = nb_BHHC,
                                       BHHT = nb_BHHT,
                                       BCC = nb_BCC,                                       
                                       BCT = nb_BCT,
                                       movements = length(movements_today),
                                       susceptPop_region = round(susceptPop_today))
           
           # Save census data to the cumulative "dailyNumberReport.txt" file
           fileName <- paste(path,"dailyNumberReport.txt",sep="/")
           write.table(t(currentCensus), fileName, append=TRUE, col.names=FALSE, row.names=FALSE, quote=FALSE, sep="\t")                

           ##################################################
           #    Save data to individual daily files         #
           ##################################################
           
           # Which files are saved is based on the value of the "save_YN" options (defined in OzRabArguments & shown below):

           contacted_dogs_today <- allDogs[which(allDogs$contactDate == simTIME),]
           if(save_contacts_YN == TRUE & nrow(contacted_dogs_today) > 0)  save_contacts(contacted_dogs_today)
           
           moved_dogs_today <- allDogs[which(allDogs$movementDate == simTIME),]
           if(save_movements_YN == TRUE & nrow(moved_dogs_today) > 0)  save_movements(moved_dogs_today)

         if (verbose) cat("summaries$day() : END\n")
         },  # end day{}
 
 
         ###############
         #             #
         #    final    #
         #             #
         ###############
         final={
         
         if (verbose) cat("summaries$final() : START\n")
         
         # save index dog(s) information
         if (save_indexDog_YN == T) {
         
         indexDogs <- allDogs$index_dog == T
         indexDogs_table <- with(allDogs,
                                 data.frame(dog_ID = dog_ID[indexDogs],
                                            hh_ID = hh_ID[indexDogs],
                                            Lat = round(Lat[indexDogs],5),
                                            Long = round(Long[indexDogs],5),
                                            Com = community[indexDogs],
                                            region = region[indexDogs]))
         
         fileName = paste(path, "indexDogs.txt", sep="/")
         write.table(indexDogs_table, fileName, row.names=F, sep="\t")
         }  
         
         # save all rabid dogs in one single table
         if (save_rabidDogs_YN == T) {
         
         rabidDogs <- allDogs$exposed == 1
         rabidDogs_table <- with (allDogs,
                                 data.frame(dog_ID = dog_ID[rabidDogs],
                                            hh_ID = hh_ID[rabidDogs],
                                            Lat = round(Lat[rabidDogs],5),
                                            Long = round(Long[rabidDogs],5),
                                            Com = community[rabidDogs],
                                            region = region[rabidDogs],
                                            expDate = contactDate[rabidDogs],
                                            infDate = infectiousDate[rabidDogs],
                                            clinDate = clinicalDate[rabidDogs],
                                            mortDate = mortalityDate[rabidDogs],
                                            movDate = movementDate[rabidDogs],
                                            vaccDate = vaccinationDate[rabidDogs],
                                            protDate = protectionDate[rabidDogs],
                                            suscept = round(susceptibility[rabidDogs],3),
                                            detDogsCull = cullDetectDogs[rabidDogs],
                                            contCull = cullContactedDog[rabidDogs],
                                            reacCull = cullReactive[rabidDogs]))
         
         fileName = paste(path, "rabidDogs.txt", sep="/")
         write.table(rabidDogs_table, fileName, row.names =F, sep="\t")                     
         }

         # save all vaccinated dogs in one single table
         if (save_vaccDogs_YN == T) {
         
         vaccDogs <- allDogs$vaccinated == 1
         vaccDogs_table <- with (allDogs,
                                 data.frame(dog_ID = dog_ID[vaccDogs],
                                            hh_ID = hh_ID[vaccDogs],
                                            Lat = round(Lat[vaccDogs],5),
                                            Long = round(Long[vaccDogs],5),
                                            Com = community[vaccDogs],
                                            region = region[vaccDogs],
                                            expDate = contactDate[vaccDogs],
                                            infDate = infectiousDate[vaccDogs],
                                            clinDate = clinicalDate[vaccDogs],
                                            mortDate = mortalityDate[vaccDogs],
                                            movDate = movementDate[vaccDogs],
                                            vaccDate = vaccinationDate[vaccDogs],
                                            protDate = protectionDate[vaccDogs],
                                            suscept = round(susceptibility[vaccDogs],3),
                                            detDogsCull = cullDetectDogs[vaccDogs],
                                            contCull = cullContactedDog[vaccDogs],
                                            reacCull = cullReactive[vaccDogs]))
         
         fileName = paste(path, "vaccDogs.txt", sep="/")
         write.table(vaccDogs_table, fileName, row.names =F, sep="\t")                     
         }
         
         # save all moved dogs in one single table
         if (save_movedDogs_YN == T) {
         
         movedDogs <- allDogs$moved == 1
         movedDogs_table <- with (allDogs,
                                 data.frame(dog_ID = dog_ID[movedDogs],
                                            hh_ID = hh_ID[movedDogs],
                                            Lat = round(Lat[movedDogs],5),
                                            Long = round(Long[movedDogs],5),
                                            Com = community[movedDogs],
                                            region = region[movedDogs],
                                            expDate = contactDate[movedDogs],
                                            infDate = infectiousDate[movedDogs],
                                            clinDate = clinicalDate[movedDogs],
                                            mortDate = mortalityDate[movedDogs],
                                            movDate = movementDate[movedDogs],
                                            vaccDate = vaccinationDate[movedDogs],
                                            protDate = protectionDate[movedDogs],
                                            suscept = round(susceptibility[movedDogs],3),
                                            detDogsCull = cullDetectDogs[movedDogs],
                                            contCull = cullContactedDog[movedDogs],
                                            reacCull = cullReactive[movedDogs]))
         
         fileName = paste(path, "movDogs.txt", sep="/")
         write.table(movedDogs_table, fileName, row.names =F, sep="\t")                     
         }
         
         # save all culled dogs in one single table
         if (save_cullDogs_YN == T) {
         
         cullDogs <- allDogs$cullDetectDogs == 1 |  allDogs$cullContactedDog == 1 | allDogs$cullReactive == 1
         cullDogs_table <- with (allDogs,
                                 data.frame(dog_ID = dog_ID[cullDogs],
                                            hh_ID = hh_ID[cullDogs],
                                            Lat = round(Lat[cullDogs],5),
                                            Long = round(Long[cullDogs],5),
                                            Com = community[cullDogs],
                                            region = region[cullDogs],
                                            expDate = contactDate[cullDogs],
                                            infDate = infectiousDate[cullDogs],
                                            clinDate = clinicalDate[cullDogs],
                                            mortDate = mortalityDate[cullDogs],
                                            movDate = movementDate[cullDogs],
                                            vaccDate = vaccinationDate[cullDogs],
                                            protDate = protectionDate[cullDogs],
                                            suscept = round(susceptibility[cullDogs],3),
                                            detDogsCull = cullDetectDogs[cullDogs],
                                            contCull = cullContactedDog[cullDogs],
                                            reacCull = cullReactive[cullDogs]))
         
         fileName = paste(path, "cullDogs.txt", sep="/")
         write.table(cullDogs_table, fileName, row.names =F, sep="\t")                     
         }

         # calculates the number of secondary cases and saves a graph depicting the number of sec cases during 
         # the outbreak
         if (save_secondaryCases_YN == T) { save_secondaryCases() } 
         
         # save a report with outbreak measures: outbreak duration, outbreak size, overall Rt
         if (save_measuresReport_YN == T) { save_measuresReport() } 
         
         # save of the epidemic curve
         if (save_epiCurves_YN == T) { save_epiCurves() }      
         
         # analyse and save parameter values together with outbreak measures in a separate file
         if (save_paramValues_YN == T) { save_paramValues() }  
         
         # save the allDogs table as a Rdata file into the main results folder
         fileName = paste(path, "allDogs.Rdata", sep="/")
         save.image(file = fileName)           
         
         if (verbose) cat("summaries$final() : END\n")
         },  # end final{}
         
         ## This will be executed if nothing above matches!
         warning("defaultSummary : 'step' should be either init, day, iter, or final")
       )

} # end standardSummary


