# runs <- readRDS("../local/runs/mstr/20250722/runs128DD.rds")

runs <- lapply(runs, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

# functions ----------
calculate_vonneumann_diff <- function(mat) {
  # Get matrix dimensions
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  
  # Create shifted versions of the matrix for each neighbor direction
  # We'll use NA padding to handle edges
  
  # North neighbor (shift down)
  north <- rbind(NA, mat[-nrows, , drop = FALSE])
  
  # South neighbor (shift up)  
  south <- rbind(mat[-1, , drop = FALSE], NA)
  
  # West neighbor (shift right)
  west <- cbind(NA, mat[, -ncols, drop = FALSE])
  
  # East neighbor (shift left)
  east <- cbind(mat[, -1, drop = FALSE], NA)
  
  # Calculate differences for each neighbor
  diff_north <- mat - north
  diff_south <- mat - south
  diff_west <- mat - west
  diff_east <- mat - east
  
  # Stack all differences into a 3D array for easy calculation
  # Each "slice" represents differences with one neighbor type
  diff_array <- array(abs(c(diff_north, diff_south, diff_west, diff_east)), 
                      dim = c(nrows, ncols, 4))
  
  # Calculate mean difference (ignoring NA values at edges)
  mean_diff <- apply(diff_array, c(1, 2), mean, na.rm = TRUE)
  
  # Handle edge cases where all neighbors are NA (corners/edges)
  # Replace NaN with NA for cleaner output
  mean_diff[is.nan(mean_diff)] <- NA
  
  return(mean_diff)
}
nicheWidthFunction <- function(x, lambda = 1, factor = 5) {
  # Exponential distribution: f(x) = lambda * exp(-lambda * x)
  # This automatically integrates to 1 over [0, infinity]
  # For x in [0, 1], we need to normalize
  
  # Calculate the normalization factor for interval [0, 1]
  norm_factor <- lambda / (1 - exp(-lambda))
  
  # Return the normalized function
  return(norm_factor * exp(-lambda * x * factor)) # factor 5 is needed, to make the kernel more specific
}
# functions ----------

runNDD <- runs$ndd1var0.01_pdd1var0.5
image(runNDD$Output$`262501`$specMat)
hist(runNDD$Output$`262501`$compMat, breaks = 1000)
hist(calculate_vonneumann_diff(runNDD$Output$`262501`$compMat), breaks = 1000)
quantile(calculate_vonneumann_diff(runNDD$Output$`262501`$compMat))

runPDD <- runs$ndd0var0.01_pdd1var0.1
image(runPDD$Output$`262501`$specMat)
hist(runPDD$Output$`262501`$compMat, breaks = 1000)
hist(calculate_vonneumann_diff(runPDD$Output$`262501`$compMat), breaks = 1000)
quantile(calculate_vonneumann_diff(runPDD$Output$`262501`$compMat))

runNeut <- runs$ndd0var0.01_pdd0var0.01
image(runNeut$Output$`262501`$specMat)
hist(runNeut$Output$`262501`$compMat, breaks = 1000)
hist(calculate_vonneumann_diff(runNeut$Output$`262501`$compMat), breaks = 1000)
quantile(calculate_vonneumann_diff(runNeut$Output$`262501`$compMat))
quantile(calculate_vonneumann_diff(runNeut$Output$`262501`$compMat), probs = 1)
# in the neutral scenario the median is 0.015
# the second peak is at 0.04

# species wise test: shows the same too
spec = unique(as.vector(runNeut$Output$`262501`$specMat))
dif_list <- list()

for (i in 1:length(spec)) {
  idx = (runNeut$Output$`262501`$specMat == spec[i])
  dif_list[[i]] <- calculate_vonneumann_diff(runNeut$Output$`262501`$compMat)[idx]
}
res <- sapply(dif_list, median)
sort(res)
