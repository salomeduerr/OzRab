#
# name: save_epiCurves
#
# This file produces and saves epidemic curves for the current iteration for
# - date of exposed dogs
# - date of infectious dogs
# - date of dead dogs
# The end of the outbreak is defined as the date the last dog died.
#
# called by: standardSummary(final)
#

save_epiCurves <- function () {

if (verbose) cat("save_epiCurves() : START\n")

# drawing of epidemic curves and save it in the results folder
#fileName <- paste(path,"/epiCurve.png", sep="")
fileName <- paste(path,"/epiCurve.pdf", sep="")
#png(file = fileName, width = 700, height = 700)
pdf(file = fileName)
par(mar = c(5, 5, 5, 2) + 0.1)

outbreak_dur <- max(allDogs$mortalityDate[allDogs$dead==1])

## build tables used to draw the epi curves
# curve 1: exposure date
exposureDate_table <- data.frame(time_vec = 0:outbreak_dur, freq_expDate = 0)
allDogs_exp <- allDogs[which(allDogs$exposed == 1 & allDogs$index_dog == F),]

for (i in unique(allDogs_exp$contactDate)) {
  cur_freq_expDate <- length(which(allDogs_exp$contactDate == i))
  exposureDate_table$freq_expDate[i] <- cur_freq_expDate
}

# curve 2: infectious date
infectiousDate_table <- data.frame(time_vec = 0:outbreak_dur, freq_infecDate = 0)
allDogs_infec <- allDogs[which(allDogs$infectious == 1),]

for (i in unique(allDogs_infec$infectiousDate)) {
  cur_freq_infecDate <- length(which(allDogs_infec$infectiousDate == i))
  infectiousDate_table$freq_infecDate[i] <- cur_freq_infecDate
}

# curve 3: mortality date
mortalityDate_table <- data.frame(time_vec = 0:outbreak_dur, freq_mortDate = 0)
allDogs_mort <- allDogs[which(allDogs$dead == 1),]

for (i in unique(allDogs_mort$mortalityDate)) {
  cur_freq_mortDate <- length(which(allDogs_mort$mortalityDate == i))
  mortalityDate_table$freq_mortDate[i] <- cur_freq_mortDate
}

# draw the curves:
min_y_axis <- max(exposureDate_table$freq_expDate, mortalityDate_table$freq_mortDate, infectiousDate_table$freq_infecDate)  # ensure that the range of the y-axis is large enough
plot(exposureDate_table$time_vec, exposureDate_table$freq_expDate, type="l", col=4,
     xlab = "time in days", ylab = "frequency", las = 1, cex.lab = 1.3,
     ylim = c(0,min_y_axis))
title (paste("epidemic curve \n", path, sep= " "), cex.main = 1.2)

lines(infectiousDate_table$time_vec, infectiousDate_table$freq_infecDate, type="l", col=2)

lines(mortalityDate_table$time_vec, mortalityDate_table$freq_mortDate, type="l", col=3)


# 4. legend
legend("topright", legend=c("Exposure","Rabid","Mortality"),
       col=c(4,2,3), bty="n", lty= 1, bg="white")

dev.off()

if (verbose) cat("save_epiCurves() : END\n")
} # end save_epiCurves()










