#
# name: choose_contact_community(comms_moved_dogs)
#
# This function defines the contact community for each dog moved today to another
# community, the probabilities of contacting another community is defined
# in the betweenCommMovementProbs matrix
#
# called by between_community_transmission()
#

choose_contact_community <- function(communities) {

if (verbose) cat("choose_contact_community() : START\n")

contactCommunities <- c()
sapply(communities, function (com_name) {

  probs <- betweenCommMovementProbs[,com_name]
  contact_com <- sample(colnames(betweenCommMovementProbs)[-1], size=1, prob= probs)
  contactCommunities <<- c(contactCommunities, contact_com)
})


if (verbose) cat("choose_contact_community() : END\n")
return(contactCommunities)
} # end choose_contact_community

