#### circular boundary within density cut region to avoid diagonal bias for density dependence ####
# in the cpp files i didnt find a decay function. If there was one, this diagonal bias limits would be senseless, as it could be controlled by the decay function (?)

library(ggplot2)
library(dplyr)
library(reshape2)

# Function to calculate density for a single position
density_update <- function(landscape, x, y, density_cutoff) {
  x_dim <- nrow(landscape)
  y_dim <- ncol(landscape)
  
  # Calculate cells within cutoff (for normalization)
  cells_within_cutoff <- sum(outer(-density_cutoff:density_cutoff, 
                                   -density_cutoff:density_cutoff, 
                                   function(x, y) sqrt(x^2 + y^2) <= density_cutoff)) - 1 # exclude center
  
  results <- data.frame()
  
  # Outer loops - iterate through focus cells
  for (X1 in -density_cutoff:density_cutoff) {
    y_lims <- floor(sqrt(density_cutoff^2 - X1^2))
    
    for (Y1 in -y_lims:y_lims) {
      # Get focus cell (with wraparound)
      focus_x <- ((x + X1 - 1 + x_dim) %% x_dim) + 1  # R is 1-indexed
      focus_y <- ((y + Y1 - 1 + y_dim) %% y_dim) + 1
      
      relatedness <- 0.0
      neighbor_count <- 0
      
      # Inner loops - examine neighbors of focus cell
      for (X2 in -density_cutoff:density_cutoff) {
        y_lims2 <- floor(sqrt(density_cutoff^2 - X2^2))
        
        for (Y2 in -y_lims2:y_lims2) {
          neighbor_x <- ((focus_x + X2 - 1 + x_dim) %% x_dim) + 1
          neighbor_y <- ((focus_y + Y2 - 1 + y_dim) %% y_dim) + 1
          
          # Skip if it's the focus cell itself
          if (!(neighbor_x == focus_x && neighbor_y == focus_y)) {
            a <- landscape[focus_x, focus_y]
            b <- landscape[neighbor_x, neighbor_y]
            diff <- abs(a - b)
            relatedness <- relatedness + diff
            neighbor_count <- neighbor_count + 1
          }
        }
      }
      
      # Store result
      local_density <- relatedness / cells_within_cutoff
      results <- rbind(results, data.frame(
        focus_x = focus_x,
        focus_y = focus_y,
        local_density = local_density,
        offset_x = X1,
        offset_y = Y1
      ))
    }
  }
  
  return(results)
}

# Function to get circular neighborhood coordinates
get_circular_neighborhood <- function(center_x, center_y, radius) {
  coords <- data.frame()
  
  for (x in -radius:radius) {
    y_lims <- floor(sqrt(radius^2 - x^2))
    for (y in -y_lims:y_lims) {
      coords <- rbind(coords, data.frame(
        x = center_x + x,
        y = center_y + y,
        offset_x = x,
        offset_y = y,
        distance = sqrt(x^2 + y^2)
      ))
    }
  }
  
  return(coords)
}

# Create example landscape
set.seed(123)
landscape_size <- 20
landscape <- matrix(runif(landscape_size^2, 0, 1), 
                    nrow = landscape_size, 
                    ncol = landscape_size)

# Parameters
center_x <- 10
center_y <- 10
density_cutoff <- 4

# Calculate density for the neighborhood
density_results <- density_update(landscape, center_x, center_y, density_cutoff)

# Get circular neighborhood for visualization
circle_coords <- get_circular_neighborhood(center_x, center_y, density_cutoff)

# Prepare landscape for plotting
landscape_df <- melt(landscape)
colnames(landscape_df) <- c("x", "y", "competition_marker")

# Create the visualization
p1 <- ggplot() +
  # Plot the landscape
  geom_tile(data = landscape_df, aes(x = x, y = y, fill = competition_marker)) +
  scale_fill_viridis_c(name = "Competition\nMarker") +
  
  # Highlight the circular neighborhood
  geom_point(data = circle_coords, aes(x = x, y = y), 
             color = "red", size = 2, alpha = 0.7) +
  
  # Mark the center point
  geom_point(aes(x = center_x, y = center_y), 
             color = "white", size = 4, shape = 21, fill = "red", stroke = 2) +
  
  coord_equal() +
  theme_minimal() +
  labs(title = paste("Circular Neighborhood (radius =", density_cutoff, ")"),
       subtitle = "Red dots show cells within the circular cutoff",
       x = "X coordinate", y = "Y coordinate")

# Create density heatmap for the calculated region
p2 <- ggplot(density_results, aes(x = focus_x, y = focus_y, fill = local_density)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Local\nDensity") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Calculated Local Density Values",
       subtitle = "Higher values = more dissimilar to neighbors",
       x = "X coordinate", y = "Y coordinate")

# Create offset visualization to show the circular pattern
p3 <- ggplot(circle_coords, aes(x = offset_x, y = offset_y, color = distance)) +
  geom_point(size = 3) +
  scale_color_viridis_c(name = "Distance\nfrom center") +
  coord_equal() +
  theme_minimal() +
  labs(title = "Circular Pattern Visualization",
       subtitle = "Shows the offset coordinates used in the algorithm",
       x = "X offset", y = "Y offset") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

# Display the plots
print(p1)
print(p2)
print(p3)

# Print some statistics
cat("Landscape size:", landscape_size, "x", landscape_size, "\n")
cat("Density cutoff radius:", density_cutoff, "\n")
cat("Cells within cutoff:", nrow(circle_coords), "\n")
cat("Center position: (", center_x, ",", center_y, ")\n")
cat("Range of local density values:", 
    round(min(density_results$local_density), 3), "to", 
    round(max(density_results$local_density), 3), "\n")