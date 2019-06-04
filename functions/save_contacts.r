#
# name: save_contacts
#
# This code produces and saves for each day a list dogs that are rabid and contacted other dogs.
# The IDs of the dogs and hh are recorded, as well as whether the contact leads to exposure or not
# and what type of contact/transmission it was (within household, between household, between community).
#
# called by: standardSummary("day")
#

save_contacts <- function (contacted_dogs) {

if (verbose) cat("save_contacts() : START\n")

contacts_today <- data.frame(contactDay = NA,
                             contacting_dog = NA,
                             contacting_hh = NA,
                             contacting_long = NA,
                             contacting_lat = NA,
                             contacting_com = NA,
                             contacted_dog = NA,
                             contacted_hh = NA,
                             contacted_long = NA,
                             contacted_lat = NA,
                             contacted_com = NA,
                             exposure = NA,
                             cont_type = NA,
                             detec_contacting_dog =NA,
                             contactCull = NA)

# loop though all the dogs that are contacted today and extract the information needed
# about the contacting dog from the allDogs table
sapply(1:nrow(contacted_dogs), function (x) {

 contacts_today[x,"contactDay"] <<- simTIME

 current_contacted_dog <- contacted_dogs[x,"dog_ID"]
 current_contacting_dog <- contacted_dogs[x, "contactDog"]

 contacts_today[x,"contacting_dog"] <<- current_contacting_dog
 contacts_today[x,"contacting_hh"] <<- allDogs$hh_ID[allDogs$dog_ID == current_contacting_dog]
 contacts_today[x,"contacting_long"] <<- allDogs$Long[allDogs$dog_ID == current_contacting_dog]
 contacts_today[x,"contacting_lat"] <<- allDogs$Lat[allDogs$dog_ID == current_contacting_dog]
 contacts_today[x,"contacting_com"] <<- allDogs$community[allDogs$dog_ID == current_contacting_dog]
 contacts_today[x,"contacted_dog"] <<- current_contacted_dog
 contacts_today[x,"contacted_hh"] <<- contacted_dogs$hh_ID[x]
 contacts_today[x,"contacted_long"] <<- contacted_dogs$Long[x]
 contacts_today[x,"contacted_lat"] <<- contacted_dogs$Lat[x]
 contacts_today[x,"contacted_com"] <<- contacted_dogs$community[x]
 contacts_today[x,"exposure"] <<- contacted_dogs$exposed[x]
 contacts_today[x,"detec_contacting_dog"] <<- allDogs$detected[allDogs$dog_ID == current_contacting_dog]
 contacts_today[x,"contactCull"] <<- contacted_dogs$cullContactedDog[x]
 
 if(str_sub(contacts_today[x,"contacting_dog"],1,2) != str_sub(contacts_today[x,"contacted_dog"],1,2)) {
  contacts_today[x,"cont_type"] <<- "BC"
  } else if (contacts_today[x,"contacting_hh"] == contacts_today[x,"contacted_hh"]) {
    contacts_today[x,"cont_type"] <<- "WHH" 
    } else {
      contacts_today[x,"cont_type"] <<- "BHH"
      }
})

fileName <- paste(contactDataPath,"/contacts_Day",simTIME,".txt", sep="")
write.table(contacts_today, fileName, row.names=F, sep="\t")

if (verbose) cat("save_contacts() : END\n")
} # end save_contacts()