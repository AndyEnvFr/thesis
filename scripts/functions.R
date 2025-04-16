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
spec_time <- function(sim_out, thinning_factor = NULL) {
  
  sr_simu <- sapply(1:length(sim_out$Output),
                    function(x) PhyloSim::specRich(simu = sim_out,
                                                   which.result = x))
  
  if (!is.null(thinning_factor) &&
      is.numeric(thinning_factor) &&
      thinning_factor > 1) {
    idx <- seq(1, length(sr_simu), thinning_factor)
    sr_simu <- sr_simu[idx]
  } else if (!is.null(thinning_factor) &&
             is.numeric(thinning_factor)) {
    warning("thinning factor must be > 1 for effective thinning")
  }
  
  plot(sr_simu, type = "l", ylim = c(0, max(sr_simu)),
       ylab = "richness", xlab = "start : end")
}


# function to save the output either batch or single. 
save_out <- function(sim_out, path, batch = FALSE, slim = TRUE) {
  if (slim) {
    sim_out <- slim(sim_out = sim_out, batch = batch)
  }
  
  date_tag <- format(Sys.Date(), "%Y%m%d")
  
  get_unique_filename <- function(base) {
    if (!file.exists(base)) return(base)
    paste0(base, "_DOUBLENAME")
  }
  
  if (batch) {
    for (i in seq_along(sim_out)) {
      file_base <- paste0(path, date_tag, "_", sim_out[[i]]$Model$scenario)
      file_final <- get_unique_filename(file_base)
      saveRDS(sim_out[[i]], file = file_final)
    }
  } else {
    file_base <- paste0(path, date_tag, "_", sim_out$Model$scenario)
    file_final <- get_unique_filename(file_base)
    saveRDS(sim_out, file = file_final)
  }
}





for (i in seq_along(res_batch)) {
  print(i)
}

  
