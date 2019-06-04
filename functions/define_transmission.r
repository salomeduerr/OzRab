#
# name: define_transmission(contacts)
#
# This function has the today's contacts as argument and defines whether
# these contacts result in a transmission or not. The probability of trans-
# mission is depending in the user defined transmissionProb.
# The dogs with disease transmission today receive a exposed status = 1.
# It further keeps track to the number of contacts today (WHHC, BHHC, BCC)
# and number of transmissions (WHHT, BHHT, BCT); WHH = within household,
# BHH = between household, BC = between community
#
# called twice by dayLoop(), both after within_ and between_hh_transmission
#


define_transmission <- function(contacts) {

if (verbose) cat("define_transmission() : START\n")

# draw for each dog a probability that the contact will lead to an exposure
transmProbDistr <- eval(parse(text=paste("r",transmissionProb[[1]],sep="")))
transmProb <-  transmProbDistr(length(contacts),
                    as.numeric(transmissionProb[[2]]),
                    as.numeric(transmissionProb[[3]]),
                    as.numeric(transmissionProb[[4]]))

# define for each dog whether the contact leads to an exposure and update the allDogs accordingly
transmisson_YN <-  rbinom(length(contacts),1,transmProb)
allDogs$exposed[contacts] <<- transmisson_YN


# keep track of the number of contacts today:
# first extract the dog_ID and hh_ID of the contacting and contacted dogs and
# check, whether the dogs_IDs indicate that the contacting dog originates from a
# different community --> BC
# if nor BC, check whether the households are identical --> WHH
# if not WHH --> BHH

sapply(contacts, function(x) {

 contacted_dog <- allDogs[x, "dog_ID"]
 contacted_hh <- allDogs[x, "hh_ID"]
 contacting_dog <- allDogs[x,"contactDog"]
 contacting_hh <- allDogs[allDogs$dog_ID == contacting_dog,"hh_ID"]
 
 if(str_sub(contacted_dog,1,2) != str_sub(contacting_dog,1,2)) {
   nb_BCC <<- nb_BCC + 1 
  } else if (contacted_hh == contacting_hh) {
    nb_WHHC <<- nb_WHHC + 1
    } else {
      nb_BHHC <<- nb_BHHC + 1
      }
})

# keep track of the number of transmissions today:
# first extract the dog_ID and hh_ID of the contacting and contacted dogs and
# check, whether the dogs_IDs indicate that the contacting dog originates from a
# different community --> BC
# if nor BC, check whether the households are identical --> WHH
# if not WHH --> BHH

transmissions <- allDogs$ID[contacts[transmisson_YN == 1]]

sapply(transmissions, function(x) {

 contacted_dog <- allDogs[x, "dog_ID"]
 contacting_dog <- allDogs[x,"contactDog"]
 contacted_hh <- allDogs[x, "hh_ID"]
 contacting_hh <- allDogs[allDogs$dog_ID == contacting_dog,"hh_ID"]

 if(str_sub(contacted_dog,1,2) != str_sub(contacting_dog,1,2)) {
  nb_BCT <<- nb_BCT + 1
  } else if (contacted_hh == contacting_hh) {
    nb_WHHT <<- nb_WHHT + 1
    } else {
      nb_BHHT <<- nb_BHHT + 1
      }
})

if (verbose) cat("define_transmission() : END\n")
} # end define_transmission(contacts)