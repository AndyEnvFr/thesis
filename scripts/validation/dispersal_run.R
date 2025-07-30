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
nddVar_fixed <- 0.01
pddVar_fixed <- 0.5
nDensityCut_options <- c(1)
pDensityCut_options <- c(1)

# Varying parameters

pdd_fixed <- c(0, 0.5, 1)
ndd_fixed <- c(0, 0.5, 1)
dispersal_options <- c(1,2,3,4)

environment_options <- c(0)
seed_options <- c(20250730001)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (pDD in pdd_fixed) {
  for (nDD in ndd_fixed) {
  for (disp in dispersal_options) {
    for (env in environment_options) {
      for (current_seed in seed_options) {
        
        params[[param_index]] <- createCompletePar(
          x = 50,
          y = 50,
          negativeDensity = nDD, 
          nDensityCut = nDensityCut_options,
          nDDNicheWidth = nddVar_fixed,
          positiveDensity = pDD, 
          pDensityCut = pDensityCut_options,
          pDDNicheWidth = pddVar_fixed,
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
          scenario = paste0("disp", disp,
                            "_allElseFixed"),
          runs = sort(c((1:150 * 1000), (1:150 * 1000) + 1 ))
        )
        
        param_index <- param_index + 1
      }
    }
  }
  }
}

runz <- runSimulationBatch(params, parallel = 36, backup_path = "~/cyber_synch/local/runs/dispVsDD/backup/")
saveRDS(runz, "~/cyber_synch/local/runs/dispVsDD/runsDisp.rds")
