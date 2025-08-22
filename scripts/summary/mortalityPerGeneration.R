# =============================================================================
# Mortality Rate Analysis Demo
# =============================================================================
# This script demonstrates mortality patterns in spatial population models
# and validates observed replacement rates through simulation

library(PhyloSim)
library(dplyr)

# =============================================================================
# 1. Load and Process Empirical Data
# =============================================================================

# Load simulation results
runs <- readRDS("~/cyber_synch/local/runs/mstr/20250807/runs_iv.rds")
runz <- getConNeigh(runs)

# Extract mortality data from every second generation (skip transient dynamics)
idx <- seq(from = 2, to = length(runz$pdd0Var1Cut1_ndd0Var20Cut1_disp1_sr2_fbmr3000_faoM$Output), by = 2)
runsIdx <- lapply(runz, function(x) x$Output[idx])

# Calculate mortality counts per generation across runs
n_generations <- length(runsIdx[[1]])
n_runs <- length(runsIdx)
mortality_matrix <- matrix(nrow = n_generations, ncol = n_runs)

for (j in seq_along(runsIdx)) {
  for (i in seq_along(runsIdx[[j]])) {
    mortality_matrix[i, j] <- sum(runsIdx[[j]][[i]]$mortMat)
  }
}

# Summary statistics
mean_mortality_per_gen <- rowMeans(mortality_matrix)
observed_mean <- mean(mean_mortality_per_gen)

cat("Empirical Results:\n")
cat("Mean mortality per generation:", round(observed_mean, 1), "\n")
cat("Range:", round(range(mean_mortality_per_gen), 1), "\n\n")

# =============================================================================
# 2. Theoretical Validation via Simulation
# =============================================================================

# Simulate random mortality in a 50x50 grid to validate replacement patterns
simulate_mortality_replacements <- function(grid_size = 50, death_rate = 2500/3, n_sims = 100) {
  
  total_positions <- grid_size^2
  
  # Single simulation function
  single_sim <- function() {
    death_positions <- matrix(FALSE, nrow = grid_size, ncol = grid_size)
    
    for (death in 1:death_rate) {
      row <- sample(1:grid_size, 1)
      col <- sample(1:grid_size, 1)
      death_positions[row, col] <- TRUE
    }
    
    sum(death_positions)  # Count unique positions that experienced death
  }
  
  # Run multiple simulations
  results <- replicate(n_sims, single_sim())
  
  return(list(
    mean_replacements = mean(results),
    sd_replacements = sd(results),
    theoretical_expectation = total_positions * (1 - exp(-death_rate / total_positions))
  ))
}

# Run simulation
set.seed(123)
sim_results <- simulate_mortality_replacements()

cat("Theoretical Validation:\n")
cat("Expected unique positions affected:", round(sim_results$theoretical_expectation, 1), "\n")
cat("Simulated mean replacements:", round(sim_results$mean_replacements, 1), "±", 
    round(sim_results$sd_replacements, 1), "\n")
