plot_combined_neighborhoods <- function(radii = c(1, 3, 5), matrix_size = 10) {
  
  # Create matrix filled with zeros
  mat <- matrix(0, nrow = matrix_size, ncol = matrix_size)
  
  # Find center position
  center_x <- ceiling(matrix_size / 2)
  center_y <- ceiling(matrix_size / 2)
  
  # Mark center cell with highest value
  mat[center_x, center_y] <- length(radii) + 1
  
  # Process each radius (from smallest to largest to show innermost radius)
  for (r_idx in 1:length(radii)) {
    radius <- radii[r_idx]
    offsets <- getCircularOffsets(radius)
    
    # Mark neighbor cells for this radius
    for (i in 1:nrow(offsets)) {
      neighbor_x <- center_x + offsets$dx[i]
      neighbor_y <- center_y + offsets$dy[i]
      
      # Check if neighbor is within matrix bounds
      if (neighbor_x >= 1 && neighbor_x <= matrix_size && 
          neighbor_y >= 1 && neighbor_y <= matrix_size) {
        # Always mark with current radius (innermost radius takes priority)
        mat[neighbor_x, neighbor_y] <- r_idx
      }
    }
  }
  
  # Create custom color palette with lighter firebrick for middle radius
  colors <- c("white", "firebrick4", "darkgrey", "firebrick1", "black")
  
  # Plot using image function
  image(1:matrix_size, 1:matrix_size, mat, 
        col = colors[1:(length(radii) + 2)],
        main = "",
        xlab = "X coordinate", 
        ylab = "Y coordinate",
        axes = TRUE)
  
  # Add grid lines
  abline(h = 1:matrix_size - 0.5, col = "gray", lty = 1, lwd = 0.3)
  abline(v = 1:matrix_size - 0.5, col = "gray", lty = 1, lwd = 0.3)
  
  radiii <- c("1 nDD", "3", "5 pDD")
  
  # Create legend
  legend_labels <- c(paste("radius", radiii), "focal cell")
  legend_colors <- colors
  
  legend("topright", 
         legend = legend_labels, 
         fill = legend_colors[2:length(legend_colors)], 
         bty = "n",
         cex = 1.2)
  
  # Calculate and print neighbor counts for each radius
  neighbor_counts <- sapply(radii, function(r) nrow(getCircularOffsets(r)))
  
  return(list(matrix = mat, neighbor_counts = neighbor_counts))
}

result_combined <- plot_combined_neighborhoods(radii = c(5,3,1), matrix_size = 20)


# PDF speichern
# pdf(paste0(root, "local/figures/plot_sr-params/spatKernel.pdf"), width = 6, height = 6, useDingbats = FALSE)
# plot_combined_neighborhoods(radii = c(5,3,1), matrix_size = 20)
# dev.off()

