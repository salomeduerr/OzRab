#
# name: calculate_distance_matrix
#
# This function calculated the distances (in meters) between all pair of households with
# dogs the index region. This matrix is then further used as look up table when distances
# between individual dogs are needed.
#
# called from initialize_Simulation
#

calculate_distance_matrix <- function(all_Dogs, verbose=T) {

if (verbose) cat("calculate_distance_matrix() : START\n")

# take the subpopulation which is the region of the index dog
allDogs_region <- subset(all_Dogs, all_Dogs$region == index_region)

# create an empty distance matrix
distance_matrix <- matrix(NA, nrow = length(unique(allDogs_region$hh_ID)), 
                              ncol = length(unique(allDogs_region$hh_ID)))

colnames(distance_matrix) <- unique(allDogs_region$hh_ID)
rownames(distance_matrix) <- unique(allDogs_region$hh_ID)

# loop through all households and calculate the distance to any other household
  sapply(unique(allDogs_region$hh_ID), function(i) {

    long_i <- allDogs_region$Long[which(allDogs_region$hh_ID==i)][1]
    lat_i  <- allDogs_region$Lat[which(allDogs_region$hh_ID==i)][1]

      sapply(unique(allDogs_region$hh_ID), function (j) {

       long_j <- allDogs_region$Long[which(allDogs_region$hh_ID==j)][1]
       lat_j  <- allDogs_region$Lat[which(allDogs_region$hh_ID==j)][1]

       cur_distance <- round(spDistsN1(cbind(long_j, lat_j), c(long_i, lat_i),
                                       longlat=TRUE),
                             digits = 3) *1000    # spDistsN1 output for longlat=T is km

       distance_matrix[i,j] <<- cur_distance
       })
  })

if (verbose) cat("calculate_distance_matrix() : END\n")

return (distance_matrix)
} # end calculate_distance_matrix

