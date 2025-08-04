library(PhyloSim)
library(parallel)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server
# source(paste0(root, "/git_synch/scripts/functions.R"))

# these runs shall test the influence of seed dispersal on DD
# the hypthesis is that with low dispersal, species can not overcome their own density effects
# therefore, they are pushed back each time they disperse

### define params and run model

# Fixed parameter values
nddVar_fixed <- 0.0
pddVar_options <- c(0.5, 0.3, 0.1, 0.075, 0.05, 0.025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001)
nDensityCut_options <- c(1)
pDensityCut_options <- c(1)

# Varying parameters

pdd_fixed <- c(1)
ndd_fixed <- c(0)
dispersal_options <- c(1)

environment_options <- c(0)
seed_options <- c(202507301)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (pDD in pdd_fixed) {
  for (nDD in ndd_fixed) {
    for (disp in dispersal_options) {
      for (var in pddVar_options) {
        for (current_seed in seed_options) {
          
          params[[param_index]] <- createCompletePar(
            x = 10,
            y = 10,
            negativeDensity = nDD, 
            nDensityCut = nDensityCut_options,
            nDDNicheWidth = nddVar_fixed,
            positiveDensity = pDD, 
            pDensityCut = pDensityCut_options,
            pDDNicheWidth = var,
            dispersal = disp,
            specRate = 2,
            environment = 0,
            fitnessBaseMortalityRatio = 10,
            seed = current_seed,
            type = "base",
            protracted = 0,
            fission = 0,
            redQueen = 0,
            redQueenStrength = 0,
            airmat = 0,
            fitnessActsOn = "mortality",
            scenario = var,
            runs = 100
          )
          
          param_index <- param_index + 1
        }
      }
    }
  }
}

for (i in 1:length(params)) {
run = runSimulation(params[[i]])
  
}

# image(run$Output[[1]]$specMat)
