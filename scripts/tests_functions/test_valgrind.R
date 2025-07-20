## debug skript for memory leaks

library(PhyloSim)
library(parallel)
root <- "~/cyber_synch/" # work from uni bayreuth server

ndd_options <- c(0, 0.5, 1)
pdd_options <- c(0, 0.5, 1)
nddVar_options <- c(0.1)
pddVar_options <- c(0.1)
environment_options <- c(0)
seed_options <- c(20250701)
params1 <- list()
param_index <- 1
for (ndd in ndd_options) {
  for (pdd in pdd_options) {
    for (nddVar in nddVar_options) {
      for (pddVar in pddVar_options) {
        for (env in environment_options) {  # << neue Schleife
          for (current_seed in seed_options) {
            params1[[param_index]] <- createCompletePar(
              x = 50,
              y = 50,
              negativeDensity = ndd, nDensityCut = 1, nDDNicheWidth = nddVar,
              positiveDensity = pdd, pDensityCut = 1, pDDNicheWidth = pddVar,
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
              scenario = paste0("ndd_pdd",param_index),
              runs = c(8000,8001)#c(sort( c((1:50 * 800) ,(1:100 * 20000), (1:100 * 20000) + 1 )))
            )
            param_index <- param_index + 1
          }
        }
      }
    }
  }
}

# runz0 <- runSimulation(par = params1[[1]])
runz0 <- runSimulationBatch(pars = params1, parallel = 9,
                            backup_path = "~/cyber_synch/local/runs/fat/new/")

