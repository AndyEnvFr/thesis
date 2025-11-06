#| warning: false
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

ranCon  <- tab        # randomize only "con"
ranMort <- tab        # randomize only "mortNextGen"
ranBoth <- tab        # randomize both independently

# randomize only "con"
ranCon[["con"]] <- sample(ranCon[["con"]])

# randomize only "mortNextGen"
ranMort[["mortNextGen"]] <- sample(ranMort[["mortNextGen"]])

# randomize both independently
ranBoth[c("mortNextGen","con")] <- lapply(ranBoth[c("mortNextGen","con")], sample)

# aggregate in list
ranList <- list(ranCon, ranMort, ranBoth)
names(ranList) <- c("ranCon", "ranMort", "ranBoth")



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

saveRDS(dat_meta, paste0(root, "local/runs/plot_sr-params/metafor/datmetaRan.rds"))
saveRDS(metamod, paste0(root, "local/runs/plot_sr-params/metafor/metamodRan.rds"))
