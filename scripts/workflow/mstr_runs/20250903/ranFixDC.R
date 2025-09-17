library(PhyloSim)
library(parallel)

# root <- "~/Uni/Master/MA/"
root <- "~/cyber_synch/"

set.seed(NULL)

draws <- 150

# Zufallswerte
ndd_draws        <- runif(draws, min = 0, max = 1)
pdd_draws        <- runif(draws, min = 0, max = 1)
nddVar_draws     <- runif(draws, min = 0.01, max = 20)
pddVar_draws     <- runif(draws, min = 0.01, max = 20)
DensityCut_draws <- sample(1:5, size = draws, replace = TRUE)
seed_draws       <- as.integer(runif(draws, 1, .Machine$integer.max))
environment_draws<- rep(0, draws)

# Param-Liste bauen
params <- vector("list", length = draws)
for (i in seq_len(draws)) {
  params[[i]] <- createCompletePar(
    x = 128, y = 128,
    negativeDensity = ndd_draws[i],
    nDensityCut = DensityCut_draws[i],
    nDDNicheWidth = nddVar_draws[i],
    positiveDensity = pdd_draws[i],
    pDensityCut = DensityCut_draws[i],
    pDDNicheWidth = pddVar_draws[i],
    dispersal = 1,
    specRate = 2,
    environment = environment_draws[i],
    fitnessBaseMortalityRatio = 3000,
    seed = seed_draws[i],
    type = "base",
    protracted = 0,
    fission = 0,
    redQueen = 0,
    redQueenStrength = 0,
    airmat = 0,
    fitnessActsOn = "mortality",
    scenario = sprintf("ndd%.4f_var%.3f_pdd%.4f_var%.3f_nCut%d_pCut%d_seed%d",
                       ndd_draws[i], nddVar_draws[i],
                       pdd_draws[i], pddVar_draws[i],
                       DensityCut_draws[i], DensityCut_draws[i], seed_draws[i]),
    runs = sort(c((1:100 * 900), (1:100 * 900) + 1 ))
  )
}

# In 3 Blöcke aufteilen (je 50 Runs)
param_blocks <- split(params, ceiling(seq_along(params) / 50))

# Dreimal laufen lassen und speichern
for (j in seq_along(param_blocks)) {
  runz <- runSimulationBatch(
    param_blocks[[j]],
    parallel = min(length(param_blocks[[j]]), 50),
    backup_path = file.path(root, "local/runs/mstr/backups/")
  )
  
  out_file <- sprintf("ranRunsFixDC_%s.rds",
                      c("i","ii","iii")[j])
  
  saveRDS(runz, file.path(root, "local/runs/mstr/20250903", out_file))
}
