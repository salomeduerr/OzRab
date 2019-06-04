#
# name: update_charts()
#
# Updates (i.e. redraws) plots to various devices, e.g. screen and/or disk
#
# called by standardSummary("day")
#

# called from standardSummary mit update_Charts(allDogs)
update_charts <- function(dogsToPlot = stop("missing dogsToPlot argument")) {

if (verbose) cat("update_charts() : START\n")
  
# Plots to render during a realization (step==time) if it's time to render,
# i.e. either the rendering counter has reached zero or this is the first day
# (we want to have a snapshot for the initial state)

# Render epi map to screen
if (displayEpiMap) {
  # Make sure we are plotting in the right window, its name is defined in the standardSummary(init)
  dev.set(deviceNumberEpiMap) 
  produce_epimap(dogsToPlot, simTIME, showLegend=TRUE, showFullTitle=F,
                 inset=-0.35, pointSize=0.8, legendScalingFactor = 1)
}

# Save epi map to disk as PDF
if (simTIME < 10) { today <- paste("000",simTIME,sep="") } else 
if (simTIME < 100) { today <- paste("00",simTIME,sep="") } else
if (simTIME < 1000) today <- paste("0",simTIME,sep="") else
today <- simTIME

if (saveEpiMapPDF) {
 fileName <- paste(spatialDataPath,"/epimap",today,".pdf", sep="")
 pdf(file = fileName,  onefile = FALSE)
 produce_epimap(dogsToPlot, simTIME, showLegend=TRUE, showFullTitle=F,
                inset=-0.35, pointSize=0.8, legendScalingFactor = 1) 
 dev.off()
} 

# Save epi map to disk as bitmaps
if (saveEpiMapBitmap) {
 fileName <- paste(spatialDataPath,"/epimap",today,".png", sep="")
 png(file = fileName, width = 700, height = 700)
 produce_epimap(dogsToPlot, simTIME, showLegend=TRUE, showFullTitle=F,
                inset=-0.25, pointSize=1.5, legendScalingFactor = 0.7) 
 dev.off()
} 

if (verbose) cat("update_charts() : END\n")
} # end update_charts()
