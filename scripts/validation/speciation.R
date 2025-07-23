library(PhyloSim)
library(parallel)
library(dplyr)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server


# Define parameter values
# Define parameter values
ndd_options <- c(0, 0.25, 0.5, 0.75, 1)
pddVar_options <- 0.01
sr_options <- c(1,3,5)

# Initialize empty list for parameters
params <- list()
param_index <- 1

# Generate all parameter combinations
for (ndd in ndd_options) {
  for (sr in sr_options) {
    params[[param_index]] <- createCompletePar(
      x = 100,
      y = 100,
      negativeDensity = ndd,
      nDensityCut = 1,
      dispersal = 1,
      specRate = sr,
      environment = 0,
      fitnessBaseMortalityRatio = 10,
      seed = 20252207,
      type = "base",
      protracted = 0,
      fission = 0,
      redQueen = 0,
      redQueenStrength = 0,
      airmat = 0,
      fitnessActsOn = "mortality",
      scenario = paste0("ndd", ndd,
                        "sr", sr),
      runs = sort(c((1:150 * 1750), (1:150 * 1750) + 1 ))
    )
    param_index <- param_index + 1
  }
}

runs <- runSimulationBatch(pars = params, parallel = 15, backup_path = "~/cyber_synch/local/runs/speciationVsDD/backups/")
saveRDS(runs, "~/cyber_synch/local/runs/speciationVsDD/speciation.rds")
