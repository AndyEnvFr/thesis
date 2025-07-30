library(PhyloSim)

# most of the things look fine. However, if ndd and pdd are equal, they should cancel each other out. But dont.
# They should look then the same as withput ndd or pdd (like they do in local scenario)
# I couldnt find, why they dont look the same
# Debuggin requires printing fitness values (e.g., also in getSeedDispersal) to see, what is different with and without DD

ndd <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs = c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                  negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                  positiveDensity = 0,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "ndd", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
pdd <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs =c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                  negativeDensity = 0,
                  positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.5,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "pdd", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
both <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs = c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                  negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                  positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.5,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "both", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
none <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs = c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                  negativeDensity = 0,
                  positiveDensity = 0,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "none", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
noneTst <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs = c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                  negativeDensity = 0, nDensityCut = 1, nDDNicheWidth = 0.1,
                  positiveDensity = 0, pDensityCut = 1, pDDNicheWidth = 0.3,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "none", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)
bothEQ <- createCompletePar(x = 50, y = 50, type = "base", dispersal = "global", runs =c(0,1,2,3,seq(1000,50000,1000)), specRate = 2,
                            negativeDensity = 1, nDensityCut = 1, nDDNicheWidth = 0.01,
                            positiveDensity = 1, pDensityCut = 1, pDDNicheWidth = 0.01,
                  environment = 0, fitnessActsOn = "mortality", fitnessBaseMortalityRatio = 10,
                  seed = 20250729, scenario = "bothEQ", protracted = 0, fission = 0, redQueen = 0,
                  redQueenStrength = 0, airmat = 0)

run <- runSimulationBatch(pars = list(ndd,pdd,both,none,noneTst, bothEQ), parallel = 6)

names <- sapply(run, function(x){
  return(x$Model$scenario)
})

names(run) <- names

run <- lapply(run, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

par(mfrow = c(3,7))

for (i in (1:length(run))) {
  image(run[[i]]$Output$`0`$specMat, main = paste0(names[i], "0"))
  image(run[[i]]$Output$`1`$specMat, main = paste0(names[i], "1"))
  image(run[[i]]$Output$`2`$specMat, main = paste0(names[i], "2"))
  image(run[[i]]$Output$`3`$specMat, main = paste0(names[i], "3"))
  image(run[[i]]$Output$`1000`$specMat, main = paste0(names[i], "1 000"))
  image(run[[i]]$Output$`10000`$specMat, main = paste0(names[i], "10 000"))
  image(run[[i]]$Output$`50000`$specMat, main = paste0(names[i], "50 00"))
}

for (i in (1:length(run))) {
  image(run[[i]]$Output$`0`$traitMat, main = paste0(names[i], "0"))
  image(run[[i]]$Output$`1`$traitMat, main = paste0(names[i], "1"))
  image(run[[i]]$Output$`2`$traitMat, main = paste0(names[i], "2"))
  image(run[[i]]$Output$`3`$traitMat, main = paste0(names[i], "3"))
  image(run[[i]]$Output$`1000`$traitMat, main = paste0(names[i], "1 000"))
  image(run[[i]]$Output$`10000`$traitMat, main = paste0(names[i], "10 000"))
  image(run[[i]]$Output$`50000`$traitMat, main = paste0(names[i], "50 00"))
}

for (i in (1:length(run))) {
  image(run[[i]]$Output$`0`$compMat, main = paste0(names[i], "0"))
  image(run[[i]]$Output$`1`$compMat, main = paste0(names[i], "1"))
  image(run[[i]]$Output$`2`$compMat, main = paste0(names[i], "2"))
  image(run[[i]]$Output$`3`$compMat, main = paste0(names[i], "3"))
  image(run[[i]]$Output$`1000`$compMat, main = paste0(names[i], "1 000"))
  image(run[[i]]$Output$`10000`$compMat, main = paste0(names[i], "10 000"))
  image(run[[i]]$Output$`50000`$compMat, main = paste0(names[i], "50 00"))
}

