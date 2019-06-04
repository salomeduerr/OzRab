# 
# name: initialize_arguments
# 
# makes the arguments (parameters) global and set a random seed
# ensures that the scenario and results folder are defined
#
# called by OzRab()
# 


initialize_arguments <- function(OzRabArgs)  {

cat("initialize_arguments() : START\n")

assign("simulationArguments", OzRabArgs, env=.GlobalEnv)
if (class(OzRabArgs)=="list"){
  argnames <- names(OzRabArgs)
  for (i in 1:length(OzRabArgs)) {
    assign(argnames[i],OzRabArgs[[i]], env=.GlobalEnv)
  }
}

# Prespecified seeds can be used
if(seed == 0) {
  seed <<- runif(n=1, min=1, max=100000000)
}
 set.seed(seed)

# stop the simultion in case of undefined scenario or result path
if (is.null(scenarioDataPath)) stop("scenario folder path (scenarioDataPath) has to be defined")
if (is.null(results_folder)) stop("result folder path (results_folder) has to be defined")

cat("initialize_arguments() : END\n")

} # end initialize_arguments()