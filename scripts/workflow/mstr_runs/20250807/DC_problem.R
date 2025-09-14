library(PhyloSim)
library(parallel)

# root <- "~/Uni/Master/MA/"
root <- "~/cyber_synch/"

### define params and run model
ndd_options        <- c(1) 
pdd_options        <- c(.8) 
nddVar_options     <- c(20) 
pddVar_options     <- c(20) 
nDensityCut_opts   <- c(1, 3)
pDensityCut_opts   <- c(1, 3)
environment_options <- c(0)
seed_options        <- c(20250807)

params <- list()
param_index <- 1

for (ndd in ndd_options) {
  for (pdd in pdd_options) {
    nddVar_to_test <- if (ndd == 0) c(nddVar_options[1]) else nddVar_options
    pddVar_to_test <- if (pdd == 0) c(pddVar_options[1]) else pddVar_options
    
    for (nddVar in nddVar_to_test) {
      for (pddVar in pddVar_to_test) {
        # skip trivial cancel-out case
        if (ndd == pdd && nddVar == pddVar && ndd != 0) {
          next
        }
        
        for (nCut in nDensityCut_opts) {      
          for (pCut in pDensityCut_opts) {    
            for (env in environment_options) {
              for (current_seed in seed_options) {
                
                params[[param_index]] <- createCompletePar(
                  x = 128,
                  y = 128,
                  negativeDensity = ndd, 
                  nDensityCut = nCut, 
                  nDDNicheWidth = nddVar,
                  positiveDensity = pdd, 
                  pDensityCut = pCut,
                  pDDNicheWidth = pddVar,
                  dispersal = 1,
                  specRate = 2,
                  environment = env,
                  fitnessBaseMortalityRatio = 3000,  # entspricht ~1/3 base mortality
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
                                    "var", pddVar,
                                    "_nCut", nCut,
                                    "_pCut", pCut),
                  runs = sort(c((1:100 * 400), (1:100 * 400) + 1 ))
                )
                
                param_index <- param_index + 1
              }
            }
          }
        }
      }
    }
  }
}

runz <- runSimulationBatch(params, parallel = length(params),
                           backup_path = paste0(root, "/local/runs/mstr/backups/"))

saveRDS(runz, paste0(root, "/local/runs/mstr/20250807/runs_DC_problem.rds"))
