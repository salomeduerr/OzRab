#
# name: save_secondaryCases
#
# This file adds a new column into the allDogs table with the number of secondary cases
# for each rabid dog in the model. It saves the table with the infectious dates and the 
# number of secondary cases for the rabid dogs.
# It further produces a graph with the number of secondary
# cases over the simulation time for each individual dog and the mean as a line.
#
# called by: standardSummary(final)
#

save_secondaryCases <- function () {

if (verbose) cat("save_secondaryCases() : START\n")


# count the number of secondary cases for all dogs which are infectious at any time 
# during the model simulation and add this number to a new column into the allDogs table
relDogs <- allDogs[allDogs$infectious==1,]
sapply(1:nrow(relDogs), function(x) {
 
 ID_curDog <- relDogs[x,"dog_ID"]
 nb_secCases <- length(which(allDogs$contactDog == ID_curDog & allDogs$infectious == 1))  
 row_curDog_in_allDogs <- which(allDogs$dog_ID == ID_curDog)                           
 allDogs[row_curDog_in_allDogs,"nb_secCases"] <<- nb_secCases
})

# extract the index dog(s) and associate the generation 0 into a new column
gen_0 <- which(allDogs$index_dog == T)
allDogs[gen_0,"generation"] <<- 0

# extract ID of the index dog(s)
gen0_ID <- allDogs[gen_0,"dog_ID"]

# extract the dogs that are infected by the current generation (starting with the index dog)
# and save the generation number in a new column
for (i in 0:15) {

  gen_ID <- eval(parse(text=paste("gen",i,"_ID",sep="")))
  
  next_gen <- which(allDogs$contactDog %in% gen_ID & allDogs$infectious==1)
  allDogs[next_gen,"generation"] <<- i + 1
  
  nextGen_ID <- allDogs[next_gen,"dog_ID"]
  
  assign(paste("gen",i+1,"_ID",sep=""), nextGen_ID)
}

# save the table containing the infectious dates and the number of secondary cases
secCases_tab <- allDogs[allDogs$infectious==1, c("infectiousDate","nb_secCases","generation")]
fileName <- paste(path, "nb_secCases.txt", sep="/")
write.table(secCases_tab, fileName, row.names =F, sep="\t")

# plot the number of secondary cases per dog over the simulation time taking the dog 
# individual starting date of infectiousness as x-axis
fileName <- paste(path,"/Rt_infectTime.png", sep="")
png(file = fileName, width = 700, height = 700)
  par(mar = c(5, 5, 5, 2) + 0.1)
  par(xpd=FALSE)
  
  plot(nb_secCases ~ infectiousDate , data=allDogs, xlab = "Starting date of infectious period",
       ylab = "number of secundary cases", las = 1, pch = 20, col = "grey40")
  title(paste("number of secondary cases of all rabid dogs during the outbreak \n", path, sep=""),
        cex.main = 1) 

  # draw a line of the mean of secondary cases per infectious date       
  means_per_date <- aggregate(allDogs$nb_secCases, list(Date = allDogs$infectiousDate), mean)
  lines(means_per_date[,1],means_per_date[,2], lwd= 1.5, col=2)
  
  # add a horizontal line into the graph at y=1 (critical R0 value)
  abline(h=1, lwd=0.7, lty=1)

dev.off()

# plot boxes for the number of secondary cases per generation and save it as a graph 
# in the results folder
fileName <- paste(path,"/Rt_generation.png", sep="")
png(file = fileName, width = 700, height = 700)
  par(mar = c(5, 5, 5, 2) + 0.1)

  boxplot(nb_secCases ~ generation, data=allDogs, las=1,
          xlab = "generation", ylab = "number of secundary cases")
  title(paste("number of secondary cases per generation time"))
  abline(h=1, col="red")
dev.off()


if (verbose) cat("save_secondaryCases() : START\n")
} # end save_secondaryCases