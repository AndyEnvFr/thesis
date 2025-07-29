library(PhyloSim)

ndd <- createCompletePar(x = 50, y = 50, type = "base", dispersal = 1, runs = seq(1000,50000,1000), specRate = 2,
                  negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                  positiveDensity = 0,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "ndd", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
pdd <- createCompletePar(x = 50, y = 50, type = "base", dispersal = 1, runs = seq(1000,50000,1000), specRate = 2,
                  negativeDensity = 0,
                  positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.5,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "pdd", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
both <- createCompletePar(x = 50, y = 50, type = "base", dispersal = 1, runs = seq(1000,50000,1000), specRate = 2,
                  negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                  positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.5,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "both", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
none <- createCompletePar(x = 50, y = 50, type = "base", dispersal = 1, runs = seq(1000,50000,1000), specRate = 2,
                  negativeDensity = 0,
                  positiveDensity = 0,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "none", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
bothEQ <- createCompletePar(x = 50, y = 50, type = "base", dispersal = 1, runs = seq(1000,50000,1000), specRate = 2,
                            negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                            positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.01,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "bothEQ", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)

run <- runSimulationBatch(pars = list(ndd,pdd,both,none, bothEQ), parallel = 5)

names <- sapply(run, function(x){
  return(x$Model$scenario)
})

names(run) <- names

run <- lapply(run, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

par(mfrow = c(5,3))

for (i in (1:length(run))) {
  image(run[[i]]$Output$`1000`$specMat, main = paste0(names[i], "1 000"))
  image(run[[i]]$Output$`10000`$specMat, main = paste0(names[i], "10 000"))
  image(run[[i]]$Output$`50000`$specMat, main = paste0(names[i], "50 00"))
}

