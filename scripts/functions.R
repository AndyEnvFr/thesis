library(PhyloSim)

# function to reduce size and keep only species matrix
slim <- function(sim_out, batch){
  if (batch == FALSE) {
    for (i in 1:length(sim_out$Output)) {
      sim_out$Output[[i]]$traitMat <- NULL
      sim_out$Output[[i]]$envMat <- NULL
      sim_out$Output[[i]]$compMat <- NULL
      sim_out$Output[[i]]$neutMat <- NULL
      sim_out$Output[[i]]$phylogeny <- NULL
      sim_out$Output[[i]]$phyloTXT <- NULL
    }
  } else {
    for (j in 1:length(sim_out)) {
      for (i in 1:length(sim_out[[j]]$Output)) {
        sim_out[[j]]$Output[[i]]$traitMat <- NULL
        sim_out[[j]]$Output[[i]]$envMat <- NULL
        sim_out[[j]]$Output[[i]]$compMat <- NULL
        sim_out[[j]]$Output[[i]]$neutMat <- NULL
        sim_out[[j]]$Output[[i]]$phylogeny <- NULL
        sim_out[[j]]$Output[[i]]$phyloTXT <- NULL
      }
    }
  }
  return(sim_out)
}

# function to plot species richness over time
spec_time <- function(sim_out, ymax, thinning_factor = NULL){
  
  sr_simu <- sapply(1:length(sim_out$Output),
                    function(x) PhyloSim::specRich(simu = sim_out,
                                                   which.result = x))
  if (!is.null(thinning_factor) &&
      is.numeric(thinning_factor) &&
      thinning_factor > 1){
    idx <- seq(1, length(sr_simu), thinning_factor)
    sr_simu <- sr_simu[idx]
  }else if (!is.null(thinning_factor) &&
      is.numeric(thinning_factor)){
    warning("thinning factor must be > 1 for effective thinning")}
  
  plot(sr_simu, type = "l", ylim = c(0,ymax))
  }


# continue here: automatize the file storage depending on the scenario name

# function to save the output either batch or single. 
save_out <- function(sim_out, path){
}


scenario <- numeric(length(params))
for (i in 1:length(params)) {
  scenario[i] <- params[[i]]$scenario
}

for (i in 1:length(params)) {
  saveRDS(simu[[i]], paste0("/home/andy/cyber_synch/local/runs/20250414_",
                            scenario[i],".rds"))
}
  
  
  
  
  
