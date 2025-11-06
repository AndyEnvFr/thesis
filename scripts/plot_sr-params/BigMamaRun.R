library(PhyloSim)
library(parallel)

root <- "~/cyber_synch/"

## OPTIONS
ndd_options        <- c(0,1)   
nddVar_options     <- c(20, 5, 0.1)

pdd_fixed          <- c(0,1)
pddVar_fixed       <- 5

environment_options <- 0
seed_options        <- 20250807

## Vollständiges Parameter-Grid
grid <- expand.grid(
  ndd        = ndd_options,
  nddVar     = nddVar_options,
  pdd        = pdd_fixed,
  pddVar     = pddVar_fixed,
  env        = environment_options,
  seed       = seed_options,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

## Filter: wenn ndd == 0, dann nur nddVar == 20 behalten
grid <- subset(grid, !(ndd == 0 & nddVar != 20))

## Params-Liste bauen
params <- lapply(seq_len(nrow(grid)), function(i) {
  gi <- grid[i, ]
  
  createCompletePar(
    x = 256, y = 256,
    negativeDensity     = gi$ndd,
    nDensityCut         = 5,
    nDDNicheWidth       = gi$nddVar,
    positiveDensity     = gi$pdd,
    pDensityCut         = 1,
    pDDNicheWidth       = gi$pddVar,
    dispersal           = 1,
    specRate            = 2,
    environment         = gi$env,
    fitnessBaseMortalityRatio = 3000,
    seed                = gi$seed,
    type                = "base",
    protracted          = 0,
    fission             = 0,
    redQueen            = 0,
    redQueenStrength    = 0,
    airmat              = 0,
    fitnessActsOn       = "mortality",
    scenario = paste0(
      "ndd", gi$ndd, "var", gi$nddVar, "cut", gi$nDensityCut, "_",
      "pdd", gi$pdd, "var", gi$pddVar, "cut", gi$pDensityCut
    ),
    runs = sort(c((1:100 * 800), (1:100 * 800) + 1))
  )
})

## Batch laufen lassen
runz <- runSimulationBatch(params, parallel = length(params),
                           backup_path = file.path(root, "local/runs/plot_sr-params/backups/"))

saveRDS(runz, file.path(root, "local/runs/plot_sr-params/mat/bigMama.rds"))


