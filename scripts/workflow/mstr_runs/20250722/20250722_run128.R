library(PhyloSim)
library(parallel)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server
# source(paste0(root, "/git_synch/scripts/functions.R"))

### define params and run model

# Define parameter values
# Define parameter values
ndd_options <- c(0, 0.5, 1) # c(0, 0.5, 1)
pdd_options <- c(0, 0.5, 1) # c(0, 0.5, 1)
nddVar_options <- c(0.01, 0.1, 0.5) # c(0.3, 0.005)
pddVar_options <- c(0.01, 0.1, 0.5) # c(0.3, 0.005)
environment_options <- c(0)
seed_options <- c(20250722)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (ndd in ndd_options) {
  for (pdd in pdd_options) {
    
    # Determine which variance values to test
    nddVar_to_test <- if (ndd == 0) c(nddVar_options[1]) else nddVar_options
    pddVar_to_test <- if (pdd == 0) c(pddVar_options[1]) else pddVar_options
    
    for (nddVar in nddVar_to_test) {
      for (pddVar in pddVar_to_test) {
        
        # Skip combinations where ndd and pdd have same strength and variance
        # (they cancel each other out mathematically)
        
        # deactivated, because the cutoff is different
        
        # if (ndd == pdd && nddVar == pddVar && ndd != 0) {
        #   next
        # }
        
        for (env in environment_options) {
          for (current_seed in seed_options) {
            
            params[[param_index]] <- createCompletePar(
              x = 128,
              y = 128,
              negativeDensity = ndd, 
              nDensityCut = 3, # ACCORDING TO SCHRÖDER ET AL FIG 2
              nDDNicheWidth = nddVar,
              positiveDensity = pdd, 
              pDensityCut = 1, # ACCORDING TO SCHRÖDER ET AL FIG 2
              pDDNicheWidth = pddVar,
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
              scenario = paste0("ndd", ndd,
                                "var", nddVar,
                                "_pdd", pdd,
                                "var", pddVar),
              runs = sort(c((1:150 * 1750), (1:150 * 1750) + 1 ))
            )
            
            param_index <- param_index + 1
          }
        }
      }
    }
  }
}
runz <- runSimulationBatch(params, parallel = 49, backup_path = "~/cyber_synch/local/runs/mstr/backups/")
saveRDS(runz, "~/cyber_synch/local/runs/mstr/20250722/runs128.rds")
