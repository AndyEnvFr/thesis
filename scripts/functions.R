library(PhyloSim)

#### extract name ####

run_name <- function(runs, batch = TRUE){
  if (batch == TRUE) {
    namelist <- numeric()
    for (i in 1:length(runs)) {
      name <- paste0("dd",ifelse(runs[[i]]$Model$density == TRUE, "T", "F"),
                     runs[[i]]$Model$compStrength,
                     "_disp", ifelse(runs[[i]]$Model$dispersal == "global", "G",
                                     runs[[i]]$Model$dispersal),
                     "_sr", runs[[i]]$Model$specRate,
                     "_e", ifelse(runs[[i]]$Model$environment == TRUE, "T", "F"),
                     "_fbmr", runs[[i]]$Model$fitnessBaseMortalityRatio,
                     "_dc", runs[[i]]$Model$densityCut)
      namelist[i] <- name
    }
  } else {
      namelist <- paste0("dd", ifelse(runs$Model$density == TRUE, "T", "F"),
                     runs$Model$compStrength,
                     "_disp", ifelse(runs$Model$dispersal == "global", "G",
                                     runs$Model$dispersal),
                     "_sr", runs$Model$specRate,
                     "_e", ifelse(runs$Model$environment == TRUE, "T", "F"),
                     "_fbmr", runs$Model$fitnessBaseMortalityRatio,
                     "_dc", runs$Model$densityCut)
  }
  return(namelist)
}


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




# function to plot species richness over time -----------------------------------
sr <- function(run){
  
  # get species richness for all results
  sr <- sapply(1:length(run$Output),
                    function(x) PhyloSim::specRich(simu = run,
                                                   which.result = x))
  # get year of result
  yr <- run$Model$runs
  
  return(data.frame(year = yr, spec_rich = sr))
}

spec_time <- function(runs, thinning_factor = NULL, ymax = NULL, plot = TRUE, batch = TRUE) {

  if (!batch){ # single run
    
      result <- sr(run = runs) # calculate sr for every year
      
      main <- run_name(runs = runs, batch = FALSE) # get plot titles
      
      if (!is.null(thinning_factor) && # thinning factor
          is.numeric(thinning_factor) &&
          thinning_factor > 1) {
        idx <- seq(1, nrow(result), thinning_factor)
        result <- result[idx,]
      } else if (!is.null(thinning_factor) &&
                 is.numeric(thinning_factor)) {
        warning("thinning factor must be > 1 for effective thinning")
      }
      
      # ymax i.e., ylim applies only for batch, to make runs comparable
      
      if (plot){
        plot(result, type = "b", ylab = "richness", xlab = "start : end",
             main = main)
      }
      
      return(result)
  }
  
  if (batch){ # for batch
    
    results <- lapply(runs, sr) # for all runs in batch
    
    if (!is.null(thinning_factor) && # thinning factor
        is.numeric(thinning_factor) &&
        thinning_factor > 1) {
      idx <- seq(1, nrow(results[[1]]), thinning_factor)
      for (i in 1:length(results)) {
        results[[i]] <- results[[i]][idx, ]
      }
    } else if (!is.null(thinning_factor) &&
               is.numeric(thinning_factor)) {
      warning("thinning factor must be > 1 for effective thinning")
    }

    for (run in names(results)) {
      
      if (is.numeric(ymax) & plot) {
        plot(results[[run]]$spec_rich ~ results[[run]]$year,
           type = "b",
           cex = .4,
           ylim = c(0, ymax),
           ylab = "richness",
           xlab = "start : end",
           main = run)
        } else if(plot) {
          plot(results[[run]]$spec_rich ~ results[[run]]$year,
               type = "b",
               cex = .4,
           ylim = c(0, max(results[[run]]$spec_rich)),
           ylab = "richness",
           xlab = "start : end",
           main = run)
        }
    }
  
    return(results)
  }
}








# function to save the output either batch or single. --------------------------
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


mortality <- function(out){ # computes mortality only if generations are consecutive
  
  if (!(1 %in% diff(out$Model$runs))) {
    stop("No consecutive generations. Can't calculate mortality")
  } 
  
  # create empty mortality matrix for all generations
  for (i in 1:length(out$Output)) {
    out$Output[[i]]$mortMat <- matrix(data = NA, nrow = out$Model$x, ncol = out$Model$y)
  }
  
  for (i in 1:(length(out$Output) -1)) {
    out$Output[[i+1]]$mortMat <-
      ifelse((out$Output[[i]]$traitMat - out$Output[[i+1]]$traitMat) == 0, FALSE, TRUE)
  }
  
  return(out)
}

mortality_batch <- function(out_batch){
  for (runs in seq_along(out_batch)) {
    out_batch[[runs]] <- mortality(out_batch[[runs]])
  }
  return(out_batch)
}



#### calculate id mat ####
# each individuum gets its personal ID based on the trait values
run_id_in <- function(runs){
  rXc <- (runs$Model$x * runs$Model$y) # dimension
  id_list <- list() # stores unique id values. Will be used max once per individuum
  id_list <- lapply(1:length(runs$Output), function(x) as.integer(c(1:rXc) + x * rXc))
  
  # create empty id matrix for all generations
  for (i in 1:length(runs$Output)) {
    runs$Output[[i]]$idMat <- matrix(data = NA, nrow = runs$Model$x, ncol = runs$Model$y)
  }
  
  # fill id at the first time step
  runs$Output[[1]]$idMat[1:rXc] <- 1:rXc
  
  # if trait values remain the same in next generation (i.e., survived) the ind.
  # keeps the number else it gets a new unique number from the id_list
  for (i in 2:length(runs$Output)) {
    runs$Output[[i]]$idMat <-
      ifelse((runs$Output[[i - 1]]$traitMat - runs$Output[[i]]$traitMat) == 0,
             runs$Output[[i - 1]]$idMat,  # return original if TRUE,
             id_list[[i]]) # return new unique id
  }
  return(runs)
}

run_id <- function(runs, batch = TRUE){
  if (batch){
    for (run in seq_along(runs)) {
      runs[[run]] <- run_id_in(runs = runs[[run]])
    }
  } else {
    runs <- run_id_in(runs = runs)
  }
  return(runs)
}


# create function to enlargen matrix: torus
# it is probably easier to intially enlargen the matrix, instead of jumping to the other side of the matrix each time the neighborhood radius transgresses the matrix edges.
torus_in <- function(matrix, max_neighborhood_radius) {
  r <- max_neighborhood_radius
  
  mrow <- dim(matrix)[1]
  mcol <- dim(matrix)[2]
  mbig <- matrix(data = NA, nrow = mrow + 2 * r, ncol = mcol + 2 * r)
  big_r <- dim(mbig)[1]
  big_c <- dim(mbig)[2]
  
  # inner part of bigmat: edges of matrix in bigmatrix
  str_r <- r + 1
  end_r <- big_r - r
  str_c <- r + 1
  end_c <- big_c - r
  
  mbig[c(str_r:end_r), c(str_c:end_c)] <- matrix
  
  # ! comments relate to bigmat (e.g., upper-wrap in bigmat = lower rows in matrix)
  # outer part of bigmat: wraped matrix
  # upper wrap
  mbig[c(1:r), c(str_c:end_c)] <-
    matrix[c((mrow - r + 1):mrow), c(1:mcol)]
  # lower wrap
  mbig[c((big_r - r + 1):big_r), c(str_c:end_c)] <-
    matrix[c(1:r), c(1:mcol)]
  # left wrap
  mbig[c(str_r:end_r), c(1:r)] <-
    matrix[c(1:mrow), c((mcol - r + 1):mcol)]
  # right wrap
  mbig[c(str_r:end_r), c((big_c - r + 1):big_c)] <-
    matrix[c(1:mrow), c(1:r)]
  
  # diagonals
  # upper-left
  mbig[c(1:r), c(1:r)] <-
    matrix[c((mrow - r + 1):mrow), c((mcol - r + 1):mcol)]
  # upper-right
  mbig[c(1:r), c((big_c - r + 1):big_c)] <-
    matrix[c((mrow - r + 1):mrow), c(1:r)]
  # lower-left
  mbig[c((big_r - r + 1):big_r), c(1:r)] <-
    matrix[c(1:r), c((mcol - r + 1):mcol)]
  # lower-right
  mbig[c((big_r - r + 1):big_r), c((big_c - r + 1):big_c)] <-
    matrix[c(1:r), c(1:r)]
  
  return(mbig)
}

torus <- function(run, overwrite = FALSE, max_neighborhood_radius = NULL) {
  if (is.null(max_neighborhood_radius)) {
    r <- run$Model$densityCut
  } else {r <- max_neighborhood_radius}
  
  # calculate mortalities by default, if not available
  if (!is.matrix(run$Output[[1]]$mortMat)) {
    run <- mortality(run)
  }
  
  # calculate id by default, if not available
  if (!is.matrix(run$Output[[1]]$idMat)) {
    run <- run_id_in(run)
  }
  
  # add generation names in run$Output[["200000"]]
  names(run$Output) <- run$Model$runs
  
  if (overwrite) {
    for (i in 1:length(run$Output)) {
      run$Output[[i]]$specMat <- torus_in(matrix = run$Output[[i]]$specMat, r)
      run$Output[[i]]$traitMat <- torus_in(matrix = run$Output[[i]]$traitMat, r)
      run$Output[[i]]$envMat <- torus_in(matrix = run$Output[[i]]$envMat, r)
      run$Output[[i]]$compMat <- torus_in(matrix = run$Output[[i]]$compMat, r)
      run$Output[[i]]$neutMat <- torus_in(matrix = run$Output[[i]]$neutMat, r)
      run$Output[[i]]$mortMat <- torus_in(matrix = run$Output[[i]]$mortMat, r)
      run$Output[[i]]$idMat <- torus_in(matrix = run$Output[[i]]$idMat, r)

    }
  } else {
    for (i in 1:length(run$Output)) {
      run$Output[[i]]$specMatBig <- torus_in(matrix = run$Output[[i]]$specMat, r)
      run$Output[[i]]$traitMatBig <- torus_in(matrix = run$Output[[i]]$traitMat, r)
      run$Output[[i]]$envMatBig <- torus_in(matrix = run$Output[[i]]$envMat, r)
      run$Output[[i]]$compMatBig <- torus_in(matrix = run$Output[[i]]$compMat, r)
      run$Output[[i]]$neutMatBig <- torus_in(matrix = run$Output[[i]]$neutMat, r)
      run$Output[[i]]$mortMatBig <- torus_in(matrix = run$Output[[i]]$mortMat, r)
      run$Output[[i]]$idMatBig <- torus_in(matrix = run$Output[[i]]$idMat, r)
    }
  }
  return(run)
}

torus_batch <- function(batch, overwrite = FALSE, max_neighborhood_radius){
  if (overwrite){
  warning("matrix was overwritten from bigger torus matrix; names of matrices remain the same\n")
    }
  
  if (is.null(max_neighborhood_radius)) {
    warning("no neighborhood radius defined. Using densityCut value")
  }
  
  for (runs in seq_along(batch)) {
    batch[[runs]] <- torus(batch[[runs]], overwrite, max_neighborhood_radius)
  }
  return(batch)
}

#### von Neumann nieghbor coord ####

vNeumann <- function(focal_coord_x, focal_coord_y, order){
  
  # ensure matrix includes all neighbors
  if ((focal_coord_x - order) < 1 | (focal_coord_y - order) < 1){
    stop("Error: neighbours out of boundary")
  }
  # ensure order is min 1
  if (order < 1){
    stop("Error: order must be minimum 1")
  }
  
  # create xy matrix with coordinates of all neighbors
  fx <- focal_coord_x
  fy <- focal_coord_y

  # get manhatten distances and all combinations
  grid_coord <- expand.grid(dx = -order:order, dy = -order:order)
  manhatten <- abs(grid_coord$dx) + abs(grid_coord$dy)
  
  # filter von Neumann nieghborhood
  valid_coords <- grid_coord[manhatten <= order & manhatten > 0, ]
  
  # Calculate neighbor coordinates
  x_coords <- fx + valid_coords$dx
  y_coords <- fy + valid_coords$dy
  
  # Calculate Euclidean distances
  euclidean_dist <- sqrt(valid_coords$dx^2 + valid_coords$dy^2)
  
  # Create result matrix
  result <- matrix(c(x_coords, y_coords, euclidean_dist), 
                   ncol = 3, byrow = FALSE)
  colnames(result) <- c("x", "y", "euclidean_dist")
  
  return(result)
}


