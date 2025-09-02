library(PhyloSim)
library(parallel)

root <- "~/cyber_synch/"

## OPTIONS
pdd_options        <- seq(0, 1, by = 0.2)        # vary pDD strength
pddVar_options     <- c(20, 10, 5, 1, 0.1)       # vary pDD niche width
pDensityCut_opts   <- c(1, 3, 5)                 # vary pDD kernel

ndd_fixed          <- 1
nddVar_fixed       <- 5
nDensityCut_fixed  <- 1

environment_options <- 0
seed_options        <- 20250807

## Parameter-Grid
grid <- expand.grid(
  pdd        = pdd_options,
  pddVar     = pddVar_options,
  pDensityCut= pDensityCut_opts,
  ndd        = ndd_fixed,
  nddVar     = nddVar_fixed,
  nDensityCut= nDensityCut_fixed,
  env        = environment_options,
  seed       = seed_options,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

## Filter: wenn pdd == 0, dann nur pddVar == 20 behalten
grid <- subset(grid, !(pdd == 0 & pddVar != 20))

## Params-Liste bauen
params <- lapply(seq_len(nrow(grid)), function(i) {
  gi <- grid[i, ]
  
  createCompletePar(
    x = 128, y = 128,
    negativeDensity     = gi$ndd,
    nDensityCut         = gi$nDensityCut,
    nDDNicheWidth       = gi$nddVar,
    positiveDensity     = gi$pdd,
    pDensityCut         = gi$pDensityCut,
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

saveRDS(runz, file.path(root, "local/runs/plot_sr-params/mat/fix-NL-N-nDC.rds"))
