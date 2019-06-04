#
# name: save_paramValues
#
# This function analyses and saves the parameter value in a table together with the
# number of dead animals, rabid animals, vaccinated animals and outbreak duration.
# This is particularly useful for the sensitivity analysis.
#
# called by: standardSummary(final)
#


save_paramValues <- function() {

if (verbose) cat("save_paramValues() : START\n")

load(paste(path,"OzRabArguments.Rdata",sep="/"))

# build a data frame with name of the parameter as first column and NA as second column
paramValues <- data.frame(parameter = names(OzRabArgs), value= rep(NA,length(OzRabArgs)))

# loop throught the arguments and extract the relevant measure for each parameter:
#      the entire value in case of a single term or number,
#      the mode in case of a pert distributed parameter,
#      the mean in case of a uniform distributed parameter
for(i in 1:length(OzRabArgs)) {

 if (length(OzRabArgs[[i]]) == 1) {paramValues[i,2]<- OzRabArgs[[i]]
   } else {
 if (OzRabArgs[[i]][[1]] == "pert") paramValues[i,2]<- OzRabArgs[[i]][[3]]
 if (OzRabArgs[[i]][[1]] == "unif") paramValues[i,2]<- mean(c(as.numeric(OzRabArgs[[i]][[2]]),
                                                            as.numeric(OzRabArgs[[i]][[3]])))
 }
}

# transpose the table and lable the columns according the parameter names
paramVal_table <- t( paramValues[,2])
colnames(paramVal_table) <- paramValues[,1]

# add outbreak measures to the parameter tables
outbreak_dur <- max(allDogs$mortalityDate[allDogs$dead==1])
nb_dead_dogs <- sum(allDogs$dead)
nb_rabid_dogs <- sum(allDogs$infectious)

paramVal_table <- cbind(paramVal_table, dead = nb_dead_dogs,
                                        rabid = nb_rabid_dogs,
                                        dur = outbreak_dur)
                                        
# save the table in the main results folder
fileName <- paste(path,"parameterValues.txt",sep="/")
write.table(paramVal_table, fileName, row.names=FALSE, quote=FALSE, sep="\t")


if (verbose) cat("save_paramValues() : END\n")

} # end save_paramValues
