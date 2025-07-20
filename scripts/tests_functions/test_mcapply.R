# For large, memory-intensive simulations, use this approach:
runSimulationBatch_Safe <- function(pars, parallel = FALSE, backup_path = NULL, strip = NULL) {
  
  if (parallel != FALSE) {
    cat("For large simulations, using mclapply for better memory management\n")
    
    # Use mclapply which is more memory efficient than foreach
    library(parallel)
    
    # Limit cores for memory safety
    cores <- ifelse(is.numeric(parallel), parallel, detectCores()-1)
    
    results <- mclapply(1:length(pars), function(i) {
      cat("Starting scenario", i, "at", Sys.time(), "\n")
      
      OUT <- runSimulation(pars[[i]])
      
      # Immediate backup
      if(!is.null(backup_path)){
        name <- file.path(backup_path, paste0(pars[[i]]$scenario, ".rds"))
        saveRDS(OUT, file = name)
        cat("Saved backup for scenario", i, "\n")
      }
      
      # Strip if requested
      if(!is.null(strip) && strip == "summaries") {
        OUT <- OUT$Output[[1]]$summaries
        class(OUT) <- "list"
      }
      
      cat("Completed scenario", i, "at", Sys.time(), "\n")
      return(OUT)
      
    }, mc.cores = cores)
    
    return(results)
  } else {
    # Sequential processing
    cat("Running sequentially\n")
    results <- list()
    for(i in 1:length(pars)) {
      results[[i]] <- runSimulation(pars[[i]])
    }
    return(results)
  }
}