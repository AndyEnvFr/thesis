library(PhyloSim)
library(parallel)

root <- "~/cyber_synch/"

params <- createCompletePar(
    x = 128, y = 128,
    negativeDensity     = 0,
    nDensityCut         = 0,
    nDDNicheWidth       = 0,
    positiveDensity     = 0,
    pDensityCut         = 0,
    pDDNicheWidth       = 0,
    dispersal           = 1,
    specRate            = 2,
    environment         = 0,
    fitnessBaseMortalityRatio = 3000,
    seed                = 20250807,
    type                = "base",
    protracted          = 0,
    fission             = 0,
    redQueen            = 0,
    redQueenStrength    = 0,
    airmat              = 0,
    fitnessActsOn       = "mortality",
    scenario = "neutral",
    runs = sort(c((1:100 * 800), (1:100 * 800) + 1))
  )


## Batch laufen lassen
cat("running neutral scenario")
run <- runSimulation(params)
saveRDS(run, file.path(root, "local/runs/plot_sr-params/mat/neutral.rds"))