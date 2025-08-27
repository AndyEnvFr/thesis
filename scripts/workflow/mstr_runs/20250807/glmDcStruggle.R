# same as run_i but with hard coded 1/3 base mortality

library(PhyloSim)
library(parallel)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server
# source(paste0(root, "/git_synch/scripts/functions.R"))

### define params and run model
# Define parameter values
ndd_options <- c(1) # c(0, 0.5, 1)
pdd_options <- c(1,3) # c(0, 0.5, 1)
nddVar_options <- c(5) # c(0.3, 0.005)
pddVar_options <- c(5) # c(0.3, 0.005)
environment_options <- c(0)
seed_options <- c(20250807)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (ndd in ndd_options) {
  for (pdd in pdd_options) {
    
    # nddVar_to_test <- if (ndd == 0) c(nddVar_options[1]) else nddVar_options
    # pddVar_to_test <- if (pdd == 0) c(pddVar_options[1]) else pddVar_options
    
    for (nddVar in nddVar_options) {
      for (pddVar in pddVar_options) {
        
        # Skip combinations where ndd and pdd have same strength and variance
        # (they cancel each other out mathematically)
        # if (ndd == pdd && nddVar == pddVar && ndd != 0) {
        #   next
        # }
        # 
        for (env in environment_options) {
          for (current_seed in seed_options) {
            
            params[[param_index]] <- createCompletePar(
              x = 100,
              y = 100,
              negativeDensity = ndd, 
              nDensityCut = pdd, 
              nDDNicheWidth = nddVar,
              positiveDensity = 0, 
              pDensityCut = 1,
              pDDNicheWidth = pddVar,
              dispersal = 1,
              specRate = 2,
              environment = 0,
              fitnessBaseMortalityRatio = 3000,
              seed = current_seed,
              type = "base",
              protracted = 0,
              fission = 0,
              redQueen = 0,
              redQueenStrength = 0,
              airmat = 0,
              fitnessActsOn = "mortality",
              scenario = paste0("_DC", pdd),
              runs = sort(c((1:150 * 100), (1:150 * 100) + 1 ))
            )
            
            param_index <- param_index + 1
          }
        }
      }
    }
  }
}

runz <- runSimulationBatch(params, parallel = length(params), backup_path = paste0(root, "/local/runs/mstr/backups/"))
saveRDS(runz, paste0(root, "/local/runs/mstr/20250807/runs_glmDcStruggle.rds"))



