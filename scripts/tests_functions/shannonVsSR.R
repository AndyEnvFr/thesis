# calculate and compare shannon diversity to species richness

library(PhyloSim)
library(parallel)
library(dplyr)
library(ggplot2)
library(vegan)

# root <- "~/Uni/Master/MA/" # work from local machine
root <- "~/cyber_synch/" # work from uni bayreuth server

# normal SR
runsRaw  <- readRDS(paste0(root, "/local/runs/plot_sr-params/mat/fix-PL-P-pDC.rds"))
S <- getSpecTime(runsRaw, plot = FALSE)
sr <- sapply(S, function(x){return(mean(x$spec_rich))})

# shannon Div
tab <- readRDS(paste0(root, "local/runs/plot_sr-params/tab/fix-P-PL-pDC.rds"))

mean_shannon <- sapply(tab, function(df) {
  shannon_values <- df %>%
    group_by(census) %>%
    summarise(shannon = diversity(table(specId), index = "shannon"),
              .groups = 'drop') %>%
    pull(shannon)
  
  mean(shannon_values)
})

names(sr) <- names(tab)


plot(scale(sr),pch = 16, col = rgb(0,0,1,.5))
points(scale(mean_shannon),pch = 16, col = rgb(0,1,0,.5))

cor(scale(sr), scale(mean_shannon))

srSort <- sort(sr, decreasing = T)
shannonSort <- sort(mean_shannon, decreasing = T)


















plot(log(datmeta$`P1-L5-C1_N0-L20-C1`$abund), datmeta$`P1-L5-C1_N0-L20-C1`$vi)
