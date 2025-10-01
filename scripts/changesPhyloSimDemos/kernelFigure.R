kernel <- function(x, lambda = 1, factor = 20) {
  norm_factor <- lambda / (1 - exp(-lambda))
  return((norm_factor * exp(-lambda * x * factor)) / 20)
}

# Second plot - remove xlab since we'll add it globally

lambda_values <- c(20, 10, 5, 2, 1, 0.5, 0.01)
x_vals <- seq(0, 0.2, 0.001)
lty_val <- c(1,1,2,1,1,1,1)

# Output PDF
pdf(paste0(root, "local/figures/plot_sr-params/expKernel.pdf"),
    width = 6, height = 6, onefile = TRUE, useDingbats = FALSE)

# Plot
plot(x_vals, rep(0, length(x_vals)), 
     type = "n", 
     xlab = "phylogenetic differences between two neighbors",
     ylab = "impact on DD effect (scalar)", 
     xlim = c(0, 0.1), 
     ylim = c(0, 1))

ii <- seq(.2,.8,.1)
cols <- rgb(ii,ii,ii,.5)
cols[1] <- "firebrick4"
cols[3] <- "firebrick1"
for (i in 1:length(lambda_values)) {
  lines(x_vals, kernel(x_vals, lambda = lambda_values[i], factor = 20), 
        col = cols[i], lwd = 3, lty = lty_val[i])
}

legend("topright", title = "lambda",
       legend = paste(lambda_values, c(" nDD", "", "   pDD", "", "", "", "")), 
       col = cols, 
       lty = lty_val, lwd = 3, 
       bty = "n", cex = 1.2)

# Close device to save
dev.off()

