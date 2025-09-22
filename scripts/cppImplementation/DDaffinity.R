# demo how the DD specificity is implemented
# 
# vH <- .01
# vL <- 0.001
# dif <- seq(0, 1, by = 0.001)
# 
# amp_high <- exp(-vH * 100)
# amp_low <- exp(-vL * 100 )
# 
# varHigh <- amp_high * exp(-0.5 * (dif / vH)^2)
# varLow <- amp_low * exp(-0.5 * (dif / vL)^2)
# 
# par(mfrow = c(1,2))
# plot(dif, varHigh, type = "l", main = paste("High Var, amp =", round(amp_high, 2)), ylim = c(0,1))
# plot(dif, varLow, type = "l", main = paste("Low Var, amp =", round(amp_low, 2)), ylim = c(0,1))
# 


# ------------------ new implementaition 2025/08/05 -------------------------

# at the basis of the magnitude of differences between the competition traits of the individuals,
# I implement a exponential kernel for the specificity of density dependent processes

# read in differences file
# dif <- read.delim("~/cyber_synch/local/debug_pdd_ndd/dif/ndd001.txt")
# range(dif)
# hist(dif$X0, breaks = 1000)


# additionally this code shows differences for long and big runs:
# notice that even in scenarios with high richness, the trait difference is < .4

# runs <- readRDS("../local/runs/mstr/20250722/runs128DD.rds")
# 
# runs <- lapply(runs, function(x){
#   names(x$Output) <- x$Model$runs
#   return(x)
# })
# 
# par(mfrow = c(1,2))
# image(runs$ndd1var0.01_pdd1var0.5$Output$`262501`$specMat)
# image(runs$ndd1var0.01_pdd1var0.5$Output$`262501`$compMat)
# range(runs$ndd1var0.01_pdd1var0.5$Output$`262501`$compMat)
# 
# image(runs$ndd0var0.01_pdd0var0.01$Output$`262501`$specMat)
# image(runs$ndd0var0.01_pdd0var0.01$Output$`262501`$compMat)
# range(runs$ndd0var0.01_pdd0var0.01$Output$`262501`$compMat)
# 
# image(runs$ndd0var0.01_pdd0.5var0.1$Output$`262501`$specMat)
# image(runs$ndd0var0.01_pdd0.5var0.1$Output$`262501`$compMat)
# range(runs$ndd0var0.01_pdd0.5var0.1$Output$`262501`$compMat)
# 
# image(runs$ndd0var0.01_pdd1var0.5$Output$`262501`$specMat)
# image(runs$ndd0var0.01_pdd1var0.5$Output$`262501`$compMat)
# range(runs$ndd0var0.01_pdd1var0.5$Output$`262501`$compMat)
# 
# for (i in (1:length(runs$ndd0var0.01_pdd0var0.01$Output))) {
#   print(diff(range(runs$ndd1var0.01_pdd1var0.5$Output[[i]]$compMat)))
# }
# for (i in (1:length(runs$ndd1var0.01_pdd1var0.5$Output))) {
#   print(diff(range(runs$ndd0var0.01_pdd0var0.01$Output[[i]]$compMat)))
# }
# for (i in (1:length(runs$ndd1var0.01_pdd1var0.5$Output))) {
#   print(diff(range(runs$ndd0var0.01_pdd0.5var0.1$Output[[i]]$compMat)))
# }
# for (i in (1:length(runs$ndd1var0.01_pdd1var0.5$Output))) {
#   print(diff(range(runs$ndd0var0.01_pdd1var0.5$Output[[i]]$compMat)))
# }

# store <- numeric(0)
# for (i in (1:length(runs$ndd1var0.01_pdd1var0.5$Output))) {
#   store[i] <- diff(range(runs$ndd1var0.01_pdd1var0.5$Output[[i]]$compMat))
# }
# mean(store)
#  interspecific trait differences can range from .15 to .4, depending on settings


# how much does one species differ ?
# #  intraspecific trait differences is < .07 through most settings
# store <- numeric(0)
# ite <- 1
# for (i in (1:length(runs$ndd1var0.1_pdd0.5var0.01$Output))) {
#   for (j in unique(as.vector(runs$ndd1var0.1_pdd0.5var0.01$Output[[i]]$specMat))) {
#     idx <-  runs$ndd1var0.1_pdd0.5var0.01$Output[[i]]$specMat == j
#     store[ite] <- diff(range(runs$ndd1var0.1_pdd0.5var0.01$Output[[i]]$compMat[idx]))
#     ite <- ite +1
#   }
# }
# quantile(store)
# hist(store, breaks = 100)


# this is the kernel function ----------------------------------------------------------------------------
nicheWidthFunction <- function(x, lambda = 1, factor = 20) {
  # Exponential distribution: f(x) = lambda * exp(-lambda * x)
  # This automatically integrates to 1 over [0, infinity]
  # For x in [0, 1], we need to normalize
  
  # Calculate the normalization factor for interval [0, 1]
  norm_factor <- lambda / (1 - exp(-lambda))
  
  # Return the normalized function
  # factor 20 is needed, to make the kernel more specific
  # / 20 is needed to scale between 0 and 1
  #   this only works if lambda <= 20
  return((norm_factor * exp(-lambda * x * factor)) / 20) 
}
# -----------------------------------------------------------------------------------------------------------------

# why factor 20 ?
par(mfrow = c(1,1), mar = c(6,6,6,1))
plot(nicheWidthFunction(seq(0,1,0.001),lambda = 20, factor = 20), type = "l",
     x = seq(0,1,0.001), xlab = "difference", ylab = "lambda", main = "factor 20")
lines(nicheWidthFunction(seq(0,1,0.001), lambda = 10, factor = 20),x = seq(0,1,0.001))
lines(nicheWidthFunction(seq(0,1,0.001),lambda = 5, factor = 20), x = seq(0,1,0.001))
lines(nicheWidthFunction(seq(0,1,0.001),lambda = 2, factor = 20), x = seq(0,1,0.001))
lines(nicheWidthFunction(seq(0,1,0.001),lambda = 1, factor = 20), x = seq(0,1,0.001))
lines(nicheWidthFunction(seq(0,1,0.001),lambda = .5, factor = 20), x = seq(0,1,0.001))
lines(nicheWidthFunction(seq(0,1,0.001),lambda = .01, factor = 20), x = seq(0,1,0.001))
abline(v = 0.04, lty = 3)
abline(v = 0.15, lty = 1, col = "red" )


