library(PhyloSim)

# function to reduce size and keep only species matrix
slim <- function(sim_out, batch){
  if (batch == FALSE) {
    for (i in 1:length(sim_out$Output)) {
      sim_out$Output[[i]]$traitMat <- NULL
      sim_out$Output[[i]]$envMat <- NULL
      # sim_out$Output[[i]]$compMat <- NULL
      sim_out$Output[[i]]$neutMat <- NULL
      sim_out$Output[[i]]$phylogeny <- NULL
      sim_out$Output[[i]]$phyloTXT <- NULL
    }
  } else {
    for (j in 1:length(sim_out)) {
      for (i in 1:length(sim_out[[j]]$Output)) {
        sim_out[[j]]$Output[[i]]$traitMat <- NULL
        sim_out[[j]]$Output[[i]]$envMat <- NULL
        # sim_out[[j]]$Output[[i]]$compMat <- NULL
        sim_out[[j]]$Output[[i]]$neutMat <- NULL
        sim_out[[j]]$Output[[i]]$phylogeny <- NULL
        sim_out[[j]]$Output[[i]]$phyloTXT <- NULL
      }
    }
  }
  return(sim_out)
}

# function to plot species richness over time
spec_time <- function(sim_out, thinning_factor = NULL, ymax) {
  
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
  
  if (is.numeric(ymax)) {
    plot(sr_simu, type = "l", ylim = c(0, ymax),
         ylab = "richness", xlab = "start : end", main = sim_out$Model$scenario)
  } else {
    plot(sr_simu, type = "l", ylim = c(0, max(sr_simu)),
         ylab = "richness", xlab = "start : end", main = sim_out$Model$scenario)
  }
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

# function for save batch runs saves interim results

# outcommented version: the try catch error part seems to cause problems:
# simulation batches never finish. Problem is not there, if try catch is exclud.

        # run_save_batch__ <-       function(params,
        #                                  save_path = "~/cyber_synch/local/runs/slim/",
        #                                  cores = 1,
        #                                  slim = TRUE) {
        # 
        #   # Load the parallel package if needed
        #   if (!requireNamespace("parallel", quietly = TRUE)) {
        #     stop("The 'parallel' package is required but not installed.")
        #   }
        # 
        #   results <- parallel::mclapply(seq_along(params), function(param_index) {
        #     tryCatch({
        #       message("Processing parameter set ", param_index, "/", length(params))
        #       current_params <- params[[param_index]]
        # 
        #       # Add param_index to results for traceability
        #       result <- runSimulation(current_params)
        #       result$param_index <- param_index
        # 
        #       save_out(sim_out = result,
        #                path = save_path,
        #                batch = FALSE,
        #                slim = slim)
        # 
        #       return(result)
        #     }, error = function(e) {
        #       # Save which param_index failed
        #       error_log <- data.frame(
        #         param_indication = param_index,
        #         error_msg = e$message,
        #         timestamp = Sys.time()
        #       )
        #       saveRDS(error_log, paste0(save_path, "ERROR", param_index, ".rds"))
        #       return(NULL)
        #     })
        #   }, mc.cores = cores)
        # }
        #   
        # run_save_batch <-       function(params,
        #                                  save_path = "~/cyber_synch/local/runs/slim/",
        #                                  cores = 1,
        #                                  slim = TRUE) {
        # 
        #   results <- parallel::mclapply(seq_along(params), function(param_index) {
        #     
        #       current_params <- params[[param_index]]
        # 
        #       # Add param_index to results for traceability
        #       result <- runSimulation(current_params)
        #     
        #       save_out(sim_out = result,
        #                path = save_path,
        #                batch = FALSE,
        #                slim = slim)
        # 
        #       return(result)},
        #       mc.cores = cores)
        # 
        #   return(results)
        # }



run_save_batch <- function(params,
                                 save_path = "~/cyber_synch/local/runs/slim/",
                                 cores = 1,
                                 slim = TRUE) {
  
  # Create a log file instead of using message()
  log_file <- paste0(save_path, "batch_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  
  results <- parallel::mclapply(seq_along(params), function(param_index) {
    # Write to a file instead of using message
    cat(paste0("Processing parameter set ", param_index, "/", length(params), "\n"), 
        file = log_file, append = TRUE)
    
    tryCatch({
      current_params <- params[[param_index]]
      result <- runSimulation(current_params)
      result$param_index <- param_index
      
      save_out(sim_out = result,
               path = save_path,
               batch = FALSE,
               slim = slim)
      
      return(result)
    }, error = function(e) {
      # Log error to file instead of creating separate files
      cat(paste0("ERROR in param_index ", param_index, ": ", e$message, "\n"),
          file = log_file, append = TRUE)
      return(NULL)
    })
  }, mc.cores = cores)
  
  return(results)
}
