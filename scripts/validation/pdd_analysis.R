ndd001 <- read.delim("debug_pdd_ndd/ndd001.txt", col.names = c("event", "fit", "deathChance"))
ndd05 <- read.delim("debug_pdd_ndd/ndd05.txt", col.names = c("event", "fit", "deathChance"))
pdd001 <- read.delim("debug_pdd_ndd/pdd001.txt", col.names = c("event", "fit", "deathChance"))
pdd05 <- read.delim("debug_pdd_ndd/pdd05.txt", col.names = c("event", "fit", "deathChance"))

plot(ndd001$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")

par(mfrow = c(2,4))
plot(ndd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "ndd1Var0.01")
hist(ndd001$event, breaks = 300)
text(nrow(ndd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
plot(ndd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "ndd1Var0.5")
hist(ndd05$event, breaks = 300)
text(nrow(ndd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.01")
hist(pdd001$event, breaks = 400)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5")
hist(pdd05$event, breaks = 500)
text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

par(mfrow = c(1,1))


# ---------------------------------  new implementation --------------------------------------------------------------

ndd001 <- read.delim("debug_pdd_ndd/fix_day/new/ndd001.txt", col.names = c("event", "fit", "deathChance", "unrel", "bin"))
ndd05 <- read.delim("debug_pdd_ndd/fix_day/new/ndd05.txt", col.names = c("event", "fit", "deathChance", "unrel", "bin"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new/pdd001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new/pdd05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

plot(ndd001$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")

par(mfrow = c(4,3))
plot(ndd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "ndd1Var0.01", ylim = c(0,1))
plot(ndd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "ndd1Var0.01", ylim = c(0,1))
hist(ndd001$event, breaks = 600)
text(nrow(ndd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "ndd1Var0.5", ylim = c(0,1))
plot(ndd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "ndd1Var0.5", ylim = c(0,1))
hist(ndd05$event, breaks = 600)
text(nrow(ndd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.01", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.01", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
hist(pdd05$event, breaks = 600)
text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# unrealtedness

range(ndd001$unrel)
range(ndd05$unrel)
range(pdd001$unrel)
range(pdd05$unrel)

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# --------------------------------------------   1   ----------------------------------------
# ---------------------------------------------------------------------------------------------
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.5.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd03 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.3.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd01 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0075 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd005 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0025 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00075 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.00075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0005 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.0005.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00025 <- read.delim("debug_pdd_ndd/fix_day/new_1/fitness_mortality_N0_P0.00025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

par(mfrow = c(5,3), mar = c(1,3,3,1))

# plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
# plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
# hist(pdd05$event, breaks = 600)
# text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# plot(pdd03$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.3", ylim = c(0,1))
# plot(pdd03$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.3", ylim = c(1,2))
# hist(pdd03$event, breaks = 600)
# text(nrow(pdd03)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# plot(pdd01$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.1", ylim = c(0,1))
# plot(pdd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.1", ylim = c(1,2))
# hist(pdd01$event, breaks = 600)
# text(nrow(pdd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# plot(pdd0075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.075", ylim = c(0,1))
# plot(pdd0075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.075", ylim = c(1,2))
# hist(pdd0075$event, breaks = 600)
# text(nrow(pdd0075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.05", ylim = c(0,1))
# plot(pdd005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.05", ylim = c(1,2))
# hist(pdd005$event, breaks = 600)
# text(nrow(pdd005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.025", ylim = c(0,1))
plot(pdd0025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.025", ylim = c(1,2))
hist(pdd0025$event, breaks = 600)
text(nrow(pdd0025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.001", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.001", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00075", ylim = c(0,1))
plot(pdd00075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00075", ylim = c(1,2))
hist(pdd00075$event, breaks = 600)
text(nrow(pdd00075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0005", ylim = c(0,1))
plot(pdd0005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0005", ylim = c(1,2))
hist(pdd0005$event, breaks = 600)
text(nrow(pdd0005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00025", ylim = c(0,1))
plot(pdd00025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00025", ylim = c(1,2))
hist(pdd00025$event, breaks = 600)
text(nrow(pdd00025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# --------------------------------------------   10   -------------------------------------------------
# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.5.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd03 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.3.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd01 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0075 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd005 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0025 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00075 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.00075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0005 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.0005.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00025 <- read.delim("debug_pdd_ndd/fix_day/new_10/fitness_mortality_N0_P0.00025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

par(mfrow = c(5,3), mar = c(1,3,3,1))

# plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
# plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
# hist(pdd05$event, breaks = 600)
# text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd03$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.3", ylim = c(0,1))
# plot(pdd03$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.3", ylim = c(1,2))
# hist(pdd03$event, breaks = 600)
# text(nrow(pdd03)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd01$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.1", ylim = c(0,1))
# plot(pdd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.1", ylim = c(1,2))
# hist(pdd01$event, breaks = 600)
# text(nrow(pdd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd0075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.075", ylim = c(0,1))
# plot(pdd0075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.075", ylim = c(1,2))
# hist(pdd0075$event, breaks = 600)
# text(nrow(pdd0075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.05", ylim = c(0,1))
# plot(pdd005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.05", ylim = c(1,2))
# hist(pdd005$event, breaks = 600)
# text(nrow(pdd005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.025", ylim = c(0,1))
plot(pdd0025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.025", ylim = c(1,2))
hist(pdd0025$event, breaks = 600)
text(nrow(pdd0025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.001", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.001", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00075", ylim = c(0,1))
plot(pdd00075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00075", ylim = c(1,2))
hist(pdd00075$event, breaks = 600)
text(nrow(pdd00075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0005", ylim = c(0,1))
plot(pdd0005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0005", ylim = c(1,2))
hist(pdd0005$event, breaks = 600)
text(nrow(pdd0005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00025", ylim = c(0,1))
plot(pdd00025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00025", ylim = c(1,2))
hist(pdd00025$event, breaks = 600)
text(nrow(pdd00025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# --------------------------------------------   20   ----------------------------------------
# ---------------------------------------------------------------------------------------------
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.5.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd03 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.3.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd01 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0075 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd005 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0025 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00075 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.00075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0005 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.0005.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00025 <- read.delim("debug_pdd_ndd/fix_day/new_20/fitness_mortality_N0_P0.00025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

par(mfrow = c(5,3), mar = c(1,3,3,1))

# plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
# plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
# hist(pdd05$event, breaks = 600)
# text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd03$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.3", ylim = c(0,1))
# plot(pdd03$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.3", ylim = c(1,2))
# hist(pdd03$event, breaks = 600)
# text(nrow(pdd03)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd01$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.1", ylim = c(0,1))
# plot(pdd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.1", ylim = c(1,2))
# hist(pdd01$event, breaks = 600)
# text(nrow(pdd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd0075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.075", ylim = c(0,1))
# plot(pdd0075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.075", ylim = c(1,2))
# hist(pdd0075$event, breaks = 600)
# text(nrow(pdd0075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")
# 
# plot(pdd005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.05", ylim = c(0,1))
# plot(pdd005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.05", ylim = c(1,2))
# hist(pdd005$event, breaks = 600)
# text(nrow(pdd005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.025", ylim = c(0,1))
plot(pdd0025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.025", ylim = c(1,2))
hist(pdd0025$event, breaks = 600)
text(nrow(pdd0025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.001", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.001", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00075", ylim = c(0,1))
plot(pdd00075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00075", ylim = c(1,2))
hist(pdd00075$event, breaks = 600)
text(nrow(pdd00075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0005", ylim = c(0,1))
plot(pdd0005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0005", ylim = c(1,2))
hist(pdd0005$event, breaks = 600)
text(nrow(pdd0005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00025", ylim = c(0,1))
plot(pdd00025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00025", ylim = c(1,2))
hist(pdd00025$event, breaks = 600)
text(nrow(pdd00025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# --------------------------------------------   50   ----------------------------------------
# ---------------------------------------------------------------------------------------------
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.5.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd03 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.3.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd01 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0075 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd005 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0025 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00075 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.00075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0005 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.0005.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00025 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.00025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0001 <- read.delim("debug_pdd_ndd/fix_day/new_50/fitness_mortality_N0_P0.0001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

par(mfrow = c(5,3), mar = c(1,3,3,1))

plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
hist(pdd05$event, breaks = 600)
text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd03$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.3", ylim = c(0,1))
plot(pdd03$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.3", ylim = c(1,2))
hist(pdd03$event, breaks = 600)
text(nrow(pdd03)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd01$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.1", ylim = c(0,1))
plot(pdd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.1", ylim = c(1,2))
hist(pdd01$event, breaks = 600)
text(nrow(pdd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.075", ylim = c(0,1))
plot(pdd0075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.075", ylim = c(1,2))
hist(pdd0075$event, breaks = 600)
text(nrow(pdd0075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.05", ylim = c(0,1))
plot(pdd005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.05", ylim = c(1,2))
hist(pdd005$event, breaks = 600)
text(nrow(pdd005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.025", ylim = c(0,1))
plot(pdd0025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.025", ylim = c(1,2))
hist(pdd0025$event, breaks = 600)
text(nrow(pdd0025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.001", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.001", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00075", ylim = c(0,1))
plot(pdd00075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00075", ylim = c(1,2))
hist(pdd00075$event, breaks = 600)
text(nrow(pdd00075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0005", ylim = c(0,1))
plot(pdd0005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0005", ylim = c(1,2))
hist(pdd0005$event, breaks = 600)
text(nrow(pdd0005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00025", ylim = c(0,1))
plot(pdd00025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00025", ylim = c(1,2))
hist(pdd00025$event, breaks = 600)
text(nrow(pdd00025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0001", ylim = c(0,1))
plot(pdd0001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0001", ylim = c(1,2))
hist(pdd0001$event, breaks = 600)
text(nrow(pdd0001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")





# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# --------------------------------------------   100   ----------------------------------------
# ---------------------------------------------------------------------------------------------
pdd05 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.5.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd03 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.3.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd01 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0075 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd005 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.05.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0025 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd001 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00075 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.00075.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0005 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.0005.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd00025 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.00025.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))
pdd0001 <- read.delim("debug_pdd_ndd/fix_day/new_100/fitness_mortality_N0_P0.0001.txt", col.names = c("event", "fit", "deathChance", "bin", "unrel"))

par(mfrow = c(5,3), mar = c(1,3,3,1))

plot(pdd05$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.5", ylim = c(0,1))
plot(pdd05$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.5", ylim = c(1,2))
hist(pdd05$event, breaks = 600)
text(nrow(pdd05)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd03$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.3", ylim = c(0,1))
plot(pdd03$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.3", ylim = c(1,2))
hist(pdd03$event, breaks = 600)
text(nrow(pdd03)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd01$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.1", ylim = c(0,1))
plot(pdd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.1", ylim = c(1,2))
hist(pdd01$event, breaks = 600)
text(nrow(pdd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.075", ylim = c(0,1))
plot(pdd0075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.075", ylim = c(1,2))
hist(pdd0075$event, breaks = 600)
text(nrow(pdd0075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.05", ylim = c(0,1))
plot(pdd005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.05", ylim = c(1,2))
hist(pdd005$event, breaks = 600)
text(nrow(pdd005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.025", ylim = c(0,1))
plot(pdd0025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.025", ylim = c(1,2))
hist(pdd0025$event, breaks = 600)
text(nrow(pdd0025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.001", ylim = c(0,1))
plot(pdd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.001", ylim = c(1,2))
hist(pdd001$event, breaks = 600)
text(nrow(pdd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00075$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00075", ylim = c(0,1))
plot(pdd00075$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00075", ylim = c(1,2))
hist(pdd00075$event, breaks = 600)
text(nrow(pdd00075)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0005$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0005", ylim = c(0,1))
plot(pdd0005$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0005", ylim = c(1,2))
hist(pdd0005$event, breaks = 600)
text(nrow(pdd0005)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd00025$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.00025", ylim = c(0,1))
plot(pdd00025$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.00025", ylim = c(1,2))
hist(pdd00025$event, breaks = 600)
text(nrow(pdd00025)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(pdd0001$unrel/4,  xlab = "individuen\nüber gesamten run", ylab = "unrelatedness", main = "pdd1Var0.0001", ylim = c(0,1))
plot(pdd0001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd1Var0.0001", ylim = c(1,2))
hist(pdd0001$event, breaks = 600)
text(nrow(pdd0001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")











