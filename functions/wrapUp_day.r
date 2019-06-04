#
# name: wrapUp_day()
# 
# Wraps up day by updating and displaying status messages and resetting various things.
#
# called by dayLoop()
#


wrapUp_day <- function(terminationStatus) {

if (verbose) cat("wrapUp_day() : START\n")

update_statusMessage()
display_statusMessage()

# re-set all the report numbers which are used for the dailyNumberReport
nb_WHHC <<- 0
nb_WHHT <<- 0
nb_BHHC <<- 0
nb_BHHT <<- 0
nb_BCC  <<- 0
nb_BCT  <<- 0

# re-set the contacted and moved dogs today
contacted_dogs_today <<- c()
moved_dogs_today     <<- c()

# re-set the origin values of the moved dogs to NULL
moved_dogs <<- c()
origin_com <<- NULL
origin_hh <<- NULL
origin_Lat <<- NULL
origin_Long <<- NULL

# re-set the today culled rabid dogs and contacted dogs
detectDogs_culled_today <<- 0
contactedDogs_culled_today <<- 0

if (verbose) cat("wrapUp_day() : END\n")
} # end wrapUp_day()