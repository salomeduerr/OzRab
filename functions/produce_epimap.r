#
# name: produce_epimap
# 
# produces a plot per day including all dogs in the communities with contacted dogs (in gray) 
# plus the dogs contacted, exposed, infectious, clinical or dead in a separate colour; 
# the dead dogs contain those died of rabies and those culled after detection of rabies
# control measures: vaccinated dogs (devided in to high and low protection after the detection
#                   delay has expired)
#                   reactive culling: dogs culled because of reactive culling (not yet detected 
#                   with rabies
# the index dog(s) are shown as a cross
# dogs living in the same household are overlapping
#
# called by update_charts
# 

produce_epimap <- function(dogsToPlot, simTIME, showLegend=TRUE, showFullTitle=FALSE,
                  inset=-0.33, pointSize=1, legendScalingFactor=1.0,...) {

if (verbose) cat("produce_epimap() : START\n")

## First: plot all dogs of the community with contacted dogs (at day one this starts
#         with the index dog(s)
par(pty="s")                              # generates a square plotting region 
par(mar=c(8.5, 3.1, 4.1, 1.1), xpd=TRUE)  # xpd=T allows to clipp to the figure (not only plot) region
comm_contacted_dogs <- unique(allDogs$community[allDogs$contact != 0])
dogs_contacted_community <- allDogs[allDogs$community %in% comm_contacted_dogs,]
plot(dogs_contacted_community$Long, dogs_contacted_community$Lat, 
    pch=21, col="grey50", cex=pointSize, xlab="Long", ylab="Lat", 
    xlim=range(dogs_contacted_community$Long), ylim=range(dogs_contacted_community$Lat))


## Second: plot the contacted, exposed, infectious, clinical and dead dogs into the map
# contact dogs
contacted_dogs <- which(dogsToPlot$contact == 1)
xy_cont_dogs <- cbind(dogsToPlot$Long[contacted_dogs], dogsToPlot$Lat[contacted_dogs])

if (length(contacted_dogs) > 0) {
  points(xy_cont_dogs, pch=20, col="blue", cex=pointSize)
}

# exposed dogs      
exposed_dogs <- which(dogsToPlot$exposed == 1)
xy_exp_dogs <- cbind(dogsToPlot$Long[exposed_dogs], dogsToPlot$Lat[exposed_dogs])

if (length(exposed_dogs) > 0) {
  points(xy_exp_dogs, pch=20, col="violet", cex=pointSize)
}

# infectious dogs
infectious_dogs <- which(dogsToPlot$infectious == 1)
xy_infec_dogs <- cbind(dogsToPlot$Long[infectious_dogs], dogsToPlot$Lat[infectious_dogs])

if (length(infectious_dogs) > 0) {
  points(xy_infec_dogs, pch=20, col="orange", cex=pointSize)
}

# clinical dogs
clinical_dogs <- which(dogsToPlot$clinical == 1)
xy_clin_dogs <- cbind(dogsToPlot$Long[clinical_dogs], dogsToPlot$Lat[clinical_dogs])

if (length(clinical_dogs) > 0) {
  points(xy_clin_dogs, pch=20, col="red", cex=pointSize)
}

# dead dogs: includes those culled after detection of rabies
dead_dogs <- which(dogsToPlot$dead == 1)
xy_dead_dogs <- cbind(dogsToPlot$Long[dead_dogs], dogsToPlot$Lat[dead_dogs])

if (length(dead_dogs) > 0) {
  points(xy_dead_dogs, pch=20, col="grey40", cex=pointSize)
}


## Third: display the vaccinated dogs
# display the dogs vaccinated
vacc_dogs <- which(allDogs$vaccinated == 1)
xy_vacc_dogs <- cbind(allDogs$Long[vacc_dogs], allDogs$Lat[vacc_dogs])

if (length(vacc_dogs) > 0) {
  points(xy_vacc_dogs, pch=2, col="greenyellow", cex=pointSize)
}

# display the dogs protected by vaccination with different colours for the two vaccEfficacies
highProt_dogs <- which(allDogs$protected == 1 & allDogs$susceptibility <= 1-as.numeric(vaccEfficacy[[2]]))
xy_highProt_dogs <- cbind(allDogs$Long[highProt_dogs], allDogs$Lat[highProt_dogs])

if (length(highProt_dogs) > 0) {
  points(xy_highProt_dogs, pch=2, col="green4", cex=pointSize)
}

lowProt_dogs <- which(allDogs$protected == 1 & allDogs$susceptibility > 1-as.numeric(vaccEfficacy[[2]]))
xy_lowProt_dogs <- cbind(allDogs$Long[lowProt_dogs], allDogs$Lat[lowProt_dogs])

if (length(lowProt_dogs) > 0) {
  points(xy_lowProt_dogs, pch=2, col="green2", cex=pointSize)
}

## Fourth: display the culled dogs
# a) culled throught culling of contacted dogs from detected rabid dogs
contCull_dogs <- which(allDogs$cullContactedDog == 1)
xy_contCull_dogs <- cbind(allDogs$Long[contCull_dogs], allDogs$Lat[contCull_dogs])

if (length(contCull_dogs) > 0) {
  points(xy_contCull_dogs, pch=3, col="firebrick2", cex=pointSize)
}

# b) culled through reactive culling
reacCull_dogs <- which(allDogs$cullReactive == 1)
xy_reacCull_dogs <- cbind(allDogs$Long[reacCull_dogs], allDogs$Lat[reacCull_dogs])

if (length(reacCull_dogs) > 0) {
  points(xy_reacCull_dogs, pch=3, col="darkred", cex=pointSize)
}


## Fifth: Index dogs and legend
# presentation of the index dog(s)
index_dogs <- which(dogsToPlot$index_dog ==TRUE)
points(dogsToPlot$Long[index_dogs], dogsToPlot$Lat[index_dogs], pch= 4, col="black", cex = 1.25, lwd = 2)

# If applicable define and add the legend
if (showLegend) {
  
 legendText  <- c(paste("contacted dogs (", length(contacted_dogs), ")", sep=""),
                  paste("exposed dogs (", length(exposed_dogs), ")", sep=""),
                  paste("infectious dogs (", length(infectious_dogs), ")", sep=""),
                  paste("clinical dogs (", length(clinical_dogs), ")", sep=""),
                  paste("dead dogs (", length(dead_dogs), ")", sep=""),
                  paste("vaccinated dogs (", length(vacc_dogs), ")", sep=""),
                  paste("high prot dogs (", length(highProt_dogs), ")", sep=""),
                  paste("low prot dogs (", length(lowProt_dogs), ")", sep=""),
                  paste("contCull dog(s) (", length(contCull_dogs), ")", sep=""),
                  paste("reacCull dog(s) (", length(reacCull_dogs), ")", sep=""),
                  paste("index dog(s) (", length(index_dogs), ")", sep=""))
 legendColor <- c("blue","violet","orange","red","grey40","greenyellow","green4","green2","firebrick2","darkred","black")
 legendPch   <- c(rep(20,5), rep(2,3), 3, 3, 4)
 legendCex   <- (pointSize*legendScalingFactor)

  legend("bottom", inset=inset, legend=legendText, bty="n", horiz=FALSE, ncol=3,
         col=legendColor, pch=legendPch, cex=legendCex)
 }

# Add a plot title (really just a s label indicating the simulation id at this point)
if (showFullTitle) {
  title(paste(spatialDataPath,"/epimap",simTIME, sep=""))
  totoElapsedTime  <- proc.time() - startTime
  bradyBunch <- round((totoElapsedTime[3]/60),1)
  mtext(paste("Day: ",simTIME,"/",max_time,", Weekday: ",(simTIME%%7+1), ", Duration: ",bradyBunch," minutes",sep=""),line=0.25)

} else {

  title(paste("Day: ",simTIME, sep=""))
  # subtitle indicating the currently involved communities
  all_comms <- c()
  sapply(1:length(comm_contacted_dogs), function(i) {
   com <- comm_contacted_dogs[i]
   all_comms <<- paste(all_comms, com, sep="  ")  
   })
  mtext(all_comms, line=0.25)
}

if (verbose) cat("produce_epimap() : END\n")
} # end produce_epimap()

