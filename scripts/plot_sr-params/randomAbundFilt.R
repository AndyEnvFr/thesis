library(PhyloSim)
library(parallel)
library(dplyr)
library(tidyverse)
library(lattice)
library(ggplot2)
library(metafor)
library(MASS)
library(viridis)
# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server

tab <- readRDS(paste0(root, "local/runs/plot_sr-params/tab/neut.rds"))

# Maybe the slope differs with species abundance ... check this by filtering abundances and do the analysis

ranRare  <- tab %>% filter(abund < 3000)   
ranComn <- tab

# still randomize con and mort for both runs 
ranRare[c("mortNextGen","con")] <- lapply(ranRare[c("mortNextGen","con")], sample)
ranComn[c("mortNextGen","con")] <- lapply(ranComn[c("mortNextGen","con")], sample)

# aggregate in list
ranList <- list(ranRare, ranComn)
names(ranList) <- c("ranRare", "ranComn")



# continue with analysis pipeline
cores <- length(ranList)
cl <- makeCluster(cores)
clusterEvalQ(cl, {
  library(dplyr)
})
tabS <- parLapply(cl, ranList, function(x) {
  x %>%
    filter(abund > 100) %>%
    mutate(specIdCen = paste0(specId, census)) %>%
    select(-indId)
})
stopCluster(cl)


cl <- makeCluster(cores)
clusterEvalQ(cl, {
  library(PhyloSim)
})
mcS_err <- parLapply(cl, tabS, function(x) {
  specIDs <- unique(x$specIdCen)
  res <- vector("list", length(specIDs))
  i <- 1
  for (sID in specIDs) {
    dat <- x[x$specIdCen == sID, ]
    mod <- glm(mortNextGen ~ con, data = dat, family = binomial())
    sfm <- summary(mod)$coefficients
    vc <- vcov(mod)[c("(Intercept)", "con"), c("(Intercept)", "con")] # * corFac # correction factor
    mort0 <- plogis(coef(mod)[1])
    mort1 <- plogis(coef(mod)[1] + coef(mod)[2]) # * corFac) # correction factor
    res[[i]] <- list(
      specId = sID,
      abund = dat$abund[1],
      mort_change = mort1 - mort0,
      coef = coef(mod)[c(1,2)], # * c(1, corFac), # correction factor
      vcov = vc
    )
    i <- i + 1
  }
  return(res)
})
stopCluster(cl)


cl <- makeCluster(cores)
mcS_err_sim <- parLapply(cl, mcS_err, function(x){
  lapply(x, function(y){
    sim <- MASS::mvrnorm(n = 100, mu = c(y$coef[1], y$coef[2]), Sigma = y$vcov)
    mort0 <- plogis(sim[, 1])
    mort1 <- plogis(sim[, 1] + sim[, 2])
    mort_diff <- mort1 - mort0
    return(data.frame(
      abund = y$abund,
      specId = y$specId,
      mean = mean(mort_diff),
      se = sd(mort_diff),
      ci_low = quantile(mort_diff, 0.025),
      ci_high = quantile(mort_diff, 0.975)
    ))
  })
})
# Stop the cluster
stopCluster(cl)


m4 <- lapply(mcS_err_sim, function(group) {
  do.call(rbind, group)
})
m4 <- lapply(m4, function(group) {
  row.names(group) <- NULL
  group <- group %>% 
    mutate(log_N = log(abund))
  return(group)
})


dat_meta <- lapply(m4, function(x) {
  escalc(measure = "GEN", yi = mean, sei = se, slab = specId, data = x)
})


cl <- makeCluster(cores)
clusterExport(cl, varlist = c("dat_meta"), envir = environment())
metamod <- parLapply(cl, dat_meta, function(x) {
  tryCatch({
    metafor::rma(
      yi = yi,
      vi = vi,
      mods = ~ log_N,
      method = "REML",
      data = x
    )
  }, error = function(e) {
    return(NULL)  # Return NULL for failed models
  })
})
stopCluster(cl)
failed_indices <- sapply(metamod, is.null)
namesShort <- names(metamod)
metamod <- metamod[!failed_indices]
names(metamod) <- namesShort[!failed_indices]
if(any(failed_indices)) {
  cat("Failed models:", paste(namesShort[failed_indices], collapse = ", "), "\n")
}

saveRDS(dat_meta, paste0(root, "local/runs/plot_sr-params/metafor/datmetaRanAbundFilt.rds"))
saveRDS(metamod, paste0(root, "local/runs/plot_sr-params/metafor/metamodRanAbundFilt.rds"))

# --------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------ VISUALIZE ----------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------


metamod <- readRDS(paste0(root, "local/runs/plot_sr-params/metafor/metamodRanAbundFilt.rds"))
dat_meta <- readRDS(paste0(root, "local/runs/plot_sr-params/metafor/datmetaRanAbundFilt.rds"))
namesShort <- names(metamod)


extract_params <- function(nm) {
  # Split by underscore to get both parts
  parts <- strsplit(nm, "_")[[1]]
  
  # First part: P-PL-PC
  first <- parts[1]
  P  <- as.numeric(sub("P([0-9.]+).*", "\\1", first))
  PL <- as.numeric(sub(".*-L([0-9.]+).*", "\\1", first))
  PC <- as.numeric(sub(".*-C([0-9.]+)$", "\\1", first))
  
  # Second part: N-NL-NC
  second <- parts[2]
  N  <- as.numeric(sub("N([0-9.]+).*", "\\1", second))
  NL <- as.numeric(sub(".*-L([0-9.]+).*", "\\1", second))
  NC <- as.numeric(sub(".*-C([0-9.]+)$", "\\1", second))
  
  data.frame(P = P, PL = PL, PC = PC, N = N, NL = NL, NC = NC)
}

## --- Process data --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pred <- lapply(dat_meta, function(x){
  expand_grid(log_N = seq(min(x$log_N, na.rm = TRUE),
                          max(x$log_N, na.rm = TRUE),
                          length.out = 1000))
})
pred <- lapply(pred, function(x){
  x$abund <- exp(x$log_N)
  x
})
pred <- lapply(seq_along(pred), function(i){
  x <- pred[[i]]
  y <- metamod[[i]]
  cbind(x, predict(object = y, newmods = x$log_N))
})
names(pred) <- namesShort

pred_df <- do.call(rbind, lapply(seq_along(pred), function(i) {
  df <- pred[[i]]
  pars <- extract_params(names(pred)[i])
  df$scenario <- names(pred)[i]
  df$P  <- pars$P
  df$PL <- pars$PL
  df$PC <- pars$PC
  df$N  <- pars$N
  df$NL <- pars$NL
  df$NC <- pars$NC
  df
}))

if ("pred" %in% names(pred_df))   names(pred_df)[names(pred_df)=="pred"]   <- "fitted"
if ("fit"  %in% names(pred_df))   names(pred_df)[names(pred_df)=="fit"]    <- "fitted"
if ("ci.lb" %in% names(pred_df))  pred_df$ci_lower <- pred_df$ci.lb
if ("ci.ub" %in% names(pred_df))  pred_df$ci_upper <- pred_df$ci.ub


x_range <- range(pred_df$abund[pred_df$abund > 0], na.rm = TRUE)

pp <- ggplot(pred_df, aes(x = abund, y = fitted,
                          color = scenario,
                          fill = scenario,
                          linetype = scenario)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_x_log10(limits = x_range, expand = expansion(mult = 0.02)) +
  scale_color_manual(values = c("darkgrey", "firebrick")) +
  scale_fill_manual(values = c("darkgrey", "firebrick")) +
  labs(title = "",
       x = "species abundance",
       y = "stabilizing CDD (%)",
       color = "scenario",
       fill = "scenario") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  coord_cartesian(ylim = c(-0.0005, 0.0010))  +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE),
         linetype = guide_legend(reverse = TRUE))
print(pp)

# 
# pdf(paste0(root, "local/figures/plot_sr-params/slopeAbundTest.pdf"), width = 6, height = 6, onefile = TRUE, useDingbats = FALSE)
# print(pp)   # each print() → new page
# dev.off()
