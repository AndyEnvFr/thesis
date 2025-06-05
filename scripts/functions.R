library(PhyloSim)

#### extract name #### ----------------------------------------------------------------------

# this function can be applied on a batch or single run.
# It extracts the name from the model parameter directly.
# Not from Model$scenario. This way, it is more reliable.

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



# function to plot species richness over time ----------------------------------------------------

# Apllies the build in specRich function for every year / generation to track change
# of the sr or div. Can be applied on batch or single run
# options are
# thinning_factor: for runs with many years / generations one might smooth out
#   the line by thinning the data
# ymax: gives all plots the same ylim. Usefull for batch plotting
# plot = bool, controls if a plot should be plotted or only sr data returned
# batch = bool, must be true, if applied on a batch

# core function
sr <- function(run){
  
  # get species richness for all results
  sr <- sapply(1:length(run$Output),
                    function(x) PhyloSim::specRich(simu = run,
                                                   which.result = x))
  # get year of result
  yr <- run$Model$runs
  
  return(data.frame(year = yr, spec_rich = sr))
}

# wrapper
spec_time <- function(runs, thinning_factor = NULL, ymax = NULL, plot = TRUE, batch = TRUE) {

  names(runs) <- run_name(runs, batch = batch)
  
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


# calculates mortality  -----------------------------------------------------------------------
# based on the trait matrices, because traits are unique

mortality <- function(out){ # core function
  
  # computes mortality only if generations are consecutive
  if (!(1 %in% diff(out$Model$runs))) {
    stop("No consecutive generations. Can't calculate mortality")
  } 
  
  # create empty mortality matrix for all generations
  for (i in 1:length(out$Output)) {
    out$Output[[i]]$mortMat <- matrix(data = NA, nrow = out$Model$x, ncol = out$Model$y)
  }
  
  # calculates mortalities based on the unique traits per individuum
  for (i in 1:(length(out$Output) -1)) {
    out$Output[[i+1]]$mortMat <-
      ifelse((out$Output[[i]]$traitMat - out$Output[[i+1]]$traitMat) == 0, FALSE, TRUE)
  }
  
  return(out)
}

mortality_batch <- function(out_batch){ # wrapper function for batch
  for (runs in seq_along(out_batch)) {
    out_batch[[runs]] <- mortality(out_batch[[runs]])
  }
  return(out_batch)
}



# calculate id mat   -----------------------------------------------------------------------
# each individuum gets its personal ID based on the trait values.
# An ID is never repeated in a run.

run_id_in <- function(runs){ # core function
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

run_id <- function(runs, batch = TRUE){ # wrapper for batch
  if (batch){
    for (run in seq_along(runs)) {
      runs[[run]] <- run_id_in(runs = runs[[run]])
    }
  } else {
    runs <- run_id_in(runs = runs)
  }
  return(runs)
}


# create function to enlargen matrix: torus    -----------------------------------------------------------------------
# The main reason for this function: clarification.
# Using the torus function makes it easier to follow the code.
# Also, development is easy, because the matrix is actually enlarged, can be plotted and sanity checked
# less clearer version: each time boundaries are transpassed, nieghbors are searched at the other end of the matrix
#   requireing more "jumping"

# arguement max_n_radius defines the N radius
# if nothign is give, the batch function will extract densityCut value

# the torus batch function prepares the data for further analysis. Therefore, by
# default it generates id and mortality matrix, if not already there.
# it renames the generations / year: instead of 1:end, they are called as their
# actual year e.g., "1000", "1001", "2000", ...

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

# wrapper function for batch
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

# circular neighborhood radius as in phylosim cpp ----------------------------------------------------- 

# takes the neighborhood radius from the phylosim cpp code and applies
# it to a given max-radius to get the shifts in coordinates of all neighbors

# Function to get circular neighborhood offsets
get_circular_offsets <- function(neigh_radius) {
  offsets <- data.frame()
  
  for (dx in -neigh_radius:neigh_radius) {
    y_lims <- floor(sqrt(neigh_radius^2 - dx^2))
    for (dy in -y_lims:y_lims) {
      # Skip the center cell (0, 0)
      if (!(dx == 0 && dy == 0)) {
        offsets <- rbind(offsets, data.frame(
          dx = dx,
          dy = dy
        ))
      }
    }
  }
  return(offsets) # return a df with the moves needed for all matrix shifts to compare all neighbors
}

# conspecific neighbors ------------------------------------------------------------------------------ 
# get the number of conspecific neighbors for each cell for a given max-neighborhood radius
# computed my matrix shift and matric addition for every coordinate shift calculated in
# "get_circular_offsets"
# has no batch = TRUE option, because it is designed for parallel computation.
# the torus is undone by default, as it's only purpose is to get con N

# here a minimal example code for parallel computation
# cl <- makeCluster(length(runz)) # make cluster with core nb
# clusterExport(cl, c("runz", "get_con_neigh", "get_circular_offsets"))
# parallel_function <- function(run){
#   offset <- get_circular_offsets(neigh_radius = run$Model$densityCut)
#   result <- get_con_neigh(run = run, radius = run$Model$densityCut)
#   return(result)
# }
# runz <- parLapply(cl, X = runz, fun = parallel_function)
# saveRDS(runz, "~/cyber_synch/local/runs/fat/20250602_05_conN")


get_con_neigh <- function(run, radius = NULL, offsets = NULL, undo_torus = TRUE){
  
  # if no radius is given take density_cut
  if(is.null(radius)){radius <- run$Model$densityCut}
  r <- radius
  
  # if no offets are given, calculate them with r
  if(is.null(offsets)){offsets <- get_circular_offsets(r)}
  
  # if no offsets are given, calculate them
  if(is.null(offsets)){offsets <- get_circular_offsets(neigh_radius = r)}
  
  lx <- dim(run$Output[[1]]$specMat)[1]
  ly <- dim(run$Output[[1]]$specMat)[2]
  
  # get every second generation
  census <- names(run$Output)[seq(1, length(names(run$Output)), by = 2)]
  for (cen in census) {
    
    # to the start add then the offsets
    sx <- c( (1 + r) , (lx - r) ) # starting x-coord of original mat in bigmat
    sy <- c( (1 + r) , (ly - r) ) # starting y-coord of original mat in bigmat
    
    # generate a matrix for summing up conspecific neighbors (by mat addition)
    # convert back to original (i.e., small) size
    con <- matrix(0, lx - r * 2, ly - r * 2)
    
    # get actual inner mat
    inner <- run$Output[[cen]]$specMat[c(sx[1] : sx[2]), c(sy[1] : sy[2])]
    
    for (xy in 1:nrow(offsets)) {
      # get offset coordinates
      xshift <- offsets[xy,1]
      yshift <- offsets[xy,2]
      
      # add offsets
      X <- sx + xshift
      Y <- sy + yshift
      
      # crucial part: compare shifted big- and original matrix
      con <- con + ifelse(run$Output[[cen]]$specMat[c(X[1] : X[2]) ,
                                                    c(Y[1] : Y[2])] == inner,
                          1, 0)
      
    }
    run$Output[[cen]]$conNeighMat <- con
  }
  
  # after getting con undo the torus for all generations, therefore outside the cen loop
  for (all in 1:length(run$Output)) {
    
    if (undo_torus){
      run$Output[[all]]$specMat <- run$Output[[all]]$specMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$traitMat <- run$Output[[all]]$traitMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$envMat <- run$Output[[all]]$envMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$compMat <- run$Output[[all]]$compMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$neutMat <- run$Output[[all]]$neutMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$mortMat <- run$Output[[all]]$mortMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
      run$Output[[all]]$idMat <- run$Output[[all]]$idMat[c(r+1 : (lx - r * 2)), c(r+1 : (ly - r * 2))]
    }
  }
  
  return(run)
}



# matrix to table ------------------------------------------------------

# extracts census, id, mortality status, number of con neighbors

mat_to_tab <- function(run){
  
  r <- run$Model$densityCut # neighb. radius
  
  # census start (where neighbour. denisty is measured)
  census <- as.character(run$Model$runs)
  cen_idx <- seq(1, length(names(run$Output)), by = 2)
  
  # get bigmat size
  big <- dim(run$Output[[1]]$specMat)
  
  # get position of original(i.e., inner) matrix
  in_x <- c(1 + r, (big[1] - r))
  in_y <- c(1 + r, (big[2] - r))
  in_dim <- big - r * 2
  
  # create empty df
  res <- data.frame(
    "census" = NA,
    "ind_id" = NA,
    "spec_id" = NA,
    "mort" = NA,
    "con" = NA)
  
  for (c in seq_along(cen_idx)) {
    
    # create interim result df. later cbind it to res
    interim <- data.frame(
      census = integer(in_dim[1] * in_dim[2]),
      ind_id = integer(in_dim[1] * in_dim[2]),
      spec_id = integer(in_dim[1] * in_dim[2]),
      mort = numeric(in_dim[1] * in_dim[2]),
      con = integer(in_dim[1] * in_dim[2])
    )
    
    interim$census <- census[cen_idx[c]]
    
    sprint <- 1
    
    for (x in (in_x[1] : in_x[2])) {
      for (y in (in_y[1] : in_y[2])) {
        interim$ind_id[sprint] = run$Output[[cen_idx[c]]]$idMat[x,y]
        interim$spec_id[sprint] = run$Output[[cen_idx[c]]]$specMat[x,y]
        interim$con[sprint] = run$Output[[cen_idx[c]]]$conNeighMat[(x - r), (y - r)]
        interim$mort[sprint] = run$Output[[cen_idx[c] + 1]]$mortMat[x,y]
        
        sprint <- sprint + 1
      }
    }
    
    res <- rbind(res,interim)
  }
  return(res[-1,]) # del initialzier NA in res df
}
