#
# name: save_movements
#
# This code produces and saves for each day a list dogs that are a) exposed, b) moved to other
# communities AND c) stay in the new community unti they get infectious. That means they are the
# dogs that are responsible to rabies transmission between communities.
#
# called by: standardSummary("day")
#

save_movements <- function (moved_dogs) {

if (verbose) cat("save_movements() : START\n")

movements_today <- data.frame(movementDay = NA,
                              dog_ID = NA,
                              origin_hh = NA,
                              origin_Lat = NA,
                              origin_Long = NA,
                              origin_com = NA,
                              new_hh = NA,
                              new_Lat = NA,
                              new_Long = NA,
                              new_com = NA)

# loop though all the dogs that are moved today and extract the information needed
# about the original and new household and community. The origin data are defined in
# the between_community_transmission function.
sapply(1:nrow(moved_dogs), function (x) {

 movements_today[x,"movementDay"] <<- simTIME
 movements_today[x,"dog_ID"]      <<- moved_dogs[x,"dog_ID"]

 movements_today[x,"origin_hh"]   <<- origin_hh[x]
 movements_today[x,"origin_Long"] <<- origin_Long[x]
 movements_today[x,"origin_Lat"]  <<- origin_Lat[x]
 movements_today[x,"origin_com"]  <<- origin_com[x]
 
 movements_today[x,"new_hh"]      <<- moved_dogs$hh_ID[x]
 movements_today[x,"new_Long"]    <<- moved_dogs$Long[x]
 movements_today[x,"new_Lat"]     <<- moved_dogs$Lat[x]
 movements_today[x,"new_com"]     <<- moved_dogs$community[x]
})

fileName <- paste(movementDataPath,"/movements_Day",simTIME,".txt", sep="")
write.table(movements_today, fileName, row.names=F, sep="\t")


if (verbose) cat("save_movements() : END\n")
} # end save_movements()