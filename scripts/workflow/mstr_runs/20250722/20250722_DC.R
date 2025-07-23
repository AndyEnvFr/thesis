library(PhyloSim)
library(parallel)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server
# source(paste0(root, "/git_synch/scripts/functions.R"))

### define params and run model

# Fixed parameter values
ndd_fixed <- 1
pdd_fixed <- 1
nddVar_fixed <- 0.01
pddVar_fixed <- 0.5

# Varying parameters
nDensityCut_options <- c(1, 2, 3)
pDensityCut_options <- c(1, 2, 3)

environment_options <- c(0)
seed_options <- c(20250722, 20250723, 20250724, 20250725, 20250726)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (nDensityCut in nDensityCut_options) {
  for (pDensityCut in pDensityCut_options) {
    
    # Skip combinations where density cuts are equal (processes neutralize)
    if (nDensityCut == pDensityCut) {
      next
    }
    
    for (env in environment_options) {
      for (current_seed in seed_options) {
        
        params[[param_index]] <- createCompletePar(
          x = 50,
          y = 50,
          negativeDensity = ndd_fixed, 
          nDensityCut = nDensityCut,
          nDDNicheWidth = nddVar_fixed,
          positiveDensity = pdd_fixed, 
          pDensityCut = pDensityCut,
          pDDNicheWidth = pddVar_fixed,
          dispersal = 1,
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
          scenario = paste0("ndd", ndd_fixed,
                            "var", nddVar_fixed,
                            "_pdd", pdd_fixed,
                            "var", pddVar_fixed,
                            "_nCut", nDensityCut,
                            "_pCut", pDensityCut),
          runs = sort(c((1:150 * 1750), (1:150 * 1750) + 1 ))
        )
        
        param_index <- param_index + 1
      }
    }
  }
}

runz <- runSimulationBatch(params, parallel = 30, backup_path = "~/cyber_synch/local/runs/mstr/backups/")
saveRDS(runz, "~/cyber_synch/local/runs/mstr/20250722/runsDC.rds")
