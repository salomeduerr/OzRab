#
# name: define_indexDog
# used emelents: index_dog, index_hh, index_community, index_region, nb_index_dogs 
#
# defines the index dog(s) either by region, community or dog ID (user defined): 
# definition of the case(es) is hierarchical: if index_dog is defined, all other are useless (not used in the model);
# else if index_hh is defined, index_community and index_region are useless, else if index_community is defined, 
# index_region is not used in the model. However, the index_region has to be defined if any other parameter has been defined.
#
# called by initilize_Simulation
#


define_indexDog <- function(verbose=T) {

if (verbose) cat("define_indexDog() : START\n")

# first check if any index_dog ID is defined
if (any(!is.na(index_dog))) {
  
  if (verbose) cat("define_indexDog() : selected index dog(s) is /are:", index_dog, 
                   "\n defined index_hh, _community and _region are not considered \n",sep=", ")
  
  index_dog_line <- which(allDogs$dog_ID %in% index_dog)
  allDogs$index_dog[index_dog_line] <<- TRUE
  allDogs$contact[index_dog_line] <<- 9*(-1)
  allDogs$contactDate[index_dog_line] <<- 9*(-1)
  allDogs$contactDog[index_dog_line] <<- "unknown; index dog"
  allDogs$exposed[index_dog_line] <<- 1
  }

# second check if any index_hh is defined  
else if (any(!is.na(index_hh))) {

  if (verbose) cat("define_indexDog() : selected index household(s) is /are:", index_hh, 
                   "\n defined index_community and _region are not considered \n",sep=", ")

  possible_index_dogs_line <- which(allDogs$hh_ID %in% index_hh)
  if (is.na(nb_index_dogs) | nb_index_dogs < 1) cat("define_indexDog(): please define an appropriate number of index dogs")
  index_dog_line <- sample(possible_index_dogs_line, nb_index_dogs, replace = F)
  index_dog_IDs <- allDogs$dog_ID[index_dog_line]
  if (verbose) cat("define_indexDog() : selected index dog(s) is /are:", index_dog_IDs, "\n",sep=", ")  
   
  allDogs$index_dog[index_dog_line] <<- TRUE
  allDogs$contact[index_dog_line] <<- 9*(-1)
  allDogs$contactDate[index_dog_line] <<- 9*(-1)
  allDogs$contactDog[index_dog_line] <<- "unknown; index dog"
  allDogs$exposed[index_dog_line] <<- 1
  }
 
# second check if any index_community is defined  
else if (any(!is.na(index_community))) {

  if (verbose) cat("define_indexDog() : selected index community(ies) is /are:", index_community, 
                   "\n defnied index_region is not considered \n",sep=", ")

  possible_index_dogs_line <- which(allDogs$community %in% index_community)
  if (is.na(nb_index_dogs) | nb_index_dogs < 1) cat("define_indexDog(): please define an appropriate number of index dogs")
  index_dog_line <- sample(possible_index_dogs_line, nb_index_dogs, replace = F)
  index_dog_IDs <- allDogs$dog_ID[index_dog_line]
  if (verbose) cat("define_indexDog() : selected index dog(s) is /are:", index_dog_IDs, "\n",sep=", ")  
   
  allDogs$index_dog[index_dog_line] <<- TRUE
  allDogs$contact[index_dog_line] <<- 9*(-1)
  allDogs$contactDate[index_dog_line] <<- 9*(-1)
  allDogs$contactDog[index_dog_line] <<- "unknown; index dog"
  allDogs$exposed[index_dog_line] <<- 1
  }
 
# third check if any index_region is defined  
else if (any(!is.na(index_region))) {

  if (verbose) cat("define_indexDog() : selected index region(s) is /are:", index_region, "\n",sep=", ")

  possible_index_dogs_line <- which(allDogs$region %in% index_region)
  if (is.na(nb_index_dogs) | nb_index_dogs < 1) cat("define_indexDog(): please define an appropriate number of index dogs")
  index_dog_line <- sample(possible_index_dogs_line, nb_index_dogs, replace = F)
  index_dog_IDs <- allDogs$dog_ID[index_dog_line]
  if (verbose) cat("define_indexDog() : selected index dog(s) is /are:", index_dog_IDs, "\n",sep=", ")  
   
  allDogs$index_dog[index_dog_line] <<- TRUE
  allDogs$contact[index_dog_line] <<- 9*(-1)
  allDogs$contactDate[index_dog_line] <<- 9*(-1)
  allDogs$contactDog[index_dog_line] <<- "unknown; index dog"
  allDogs$exposed[index_dog_line] <<- 1
  }
  
# else, if nothing has been defined by the user the model stops as it requires at least a defined index region
else  { stop("please select at least the index region") }

if (verbose) cat("define_indexDog() : END\n")
} # end define_indexDog

