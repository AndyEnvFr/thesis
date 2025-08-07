# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 1 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------


ndd01 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P5.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P15.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/fitness_mortality_N0_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor1/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

par(mfrow = c(6,3), mar = c(1,3,3,1))
plot(ndd01$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda0.1", ylim = c(0,1))
plot(ndd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.1", ylim = c(1,2))
hist(ndd01$event, breaks = 600)
text(nrow(ndd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd1$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda1", ylim = c(0,1))
plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(1,2))
hist(ndd1$event, breaks = 600)
text(nrow(ndd1)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd5$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda5", ylim = c(0,1))
plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(1,2))
hist(ndd5$event, breaks = 600)
text(nrow(ndd5)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd10$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda10", ylim = c(0,1))
plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(1,2))
hist(ndd10$event, breaks = 600)
text(nrow(ndd10)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd15$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda15", ylim = c(0,1))
plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(1,2))
hist(ndd15$event, breaks = 600)
text(nrow(ndd15)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd20$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda20", ylim = c(0,1))
plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(1,2))
hist(ndd20$event, breaks = 600)
text(nrow(ndd20)/100, x = 50, y = 50, cex = 3, col = "lightgrey")



# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 5 ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------

ndd01 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P5.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P15.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/fitness_mortality_N0_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

par(mfrow = c(6,3), mar = c(1,3,3,1))
plot(ndd01$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda0.1", ylim = c(0,1))
plot(ndd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.1", ylim = c(1,2))
hist(ndd01$event, breaks = 600)
text(nrow(ndd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd1$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda1", ylim = c(0,1))
plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(1,2))
hist(ndd1$event, breaks = 600)
text(nrow(ndd1)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd5$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda5", ylim = c(0,1))
plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(1,2))
hist(ndd5$event, breaks = 600)
text(nrow(ndd5)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd10$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda10", ylim = c(0,1))
plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(1,2))
hist(ndd10$event, breaks = 600)
text(nrow(ndd10)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd15$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda15", ylim = c(0,1))
plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(1,2))
hist(ndd15$event, breaks = 600)
text(nrow(ndd15)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd20$P_relat/4,  xlab = "individuen\nüber gesamten run", ylab = "P_relatatedness", main = "pdd_lambda20", ylim = c(0,1))
plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(1,2))
hist(ndd20$event, breaks = 600)
text(nrow(ndd20)/100, x = 50, y = 50, cex = 3, col = "lightgrey")



# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 5 Longer Runs ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------

ndd01 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P5.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P15.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor5L/fitness_mortality_N0_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

# filter to get only the last 100 generations
l <- nrow(ndd01)
ndd01 <- ndd01[c((l-10000):l),]
l <- nrow(ndd1)
ndd1 <- ndd1[c((l-10000):l),]
l <- nrow(ndd5)
ndd5 <- ndd5[c((l-10000):l),]
l <- nrow(ndd10)
ndd10 <- ndd10[c((l-10000):l),]
l <- nrow(ndd15)
ndd15 <- ndd15[c((l-10000):l),]
l <- nrow(ndd20)
ndd20 <- ndd20[c((l-10000):l),]


par(mfrow = c(2,3), mar = c(1,3,3,1))
plot(ndd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.1", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)

plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)

plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)

plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)

plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)

plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(1,2), col = rgb(0,0,0,.1), cex = .3)


# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 20 Longer Runs ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------


ndd001 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P0.01.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd01 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P0.1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P5.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P15.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20L/fitness_mortality_N0_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

# filter to get only the last 100 generations
l <- nrow(ndd001)
ndd001 <- ndd001[c((l-10000):l),]
l <- nrow(ndd01)
ndd01 <- ndd01[c((l-10000):l),]
l <- nrow(ndd1)
ndd1 <- ndd1[c((l-10000):l),]
l <- nrow(ndd5)
ndd5 <- ndd5[c((l-10000):l),]
l <- nrow(ndd10)
ndd10 <- ndd10[c((l-10000):l),]
l <- nrow(ndd15)
ndd15 <- ndd15[c((l-10000):l),]
l <- nrow(ndd20)
ndd20 <- ndd20[c((l-10000):l),]

sum(ndd001$fit)
sum(ndd20$fit)


par(mfrow = c(7,2), mar = c(1,3,3,1))
plot(ndd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.01", ylim = c(1,2))
hist(ndd001$event, breaks = 600)
text(nrow(ndd001)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.1", ylim = c(1,2))
hist(ndd01$event, breaks = 600)
text(nrow(ndd01)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(1,2))
hist(ndd1$event, breaks = 600)
text(nrow(ndd1)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(1,2))
hist(ndd5$event, breaks = 600)
text(nrow(ndd5)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(1,2))
hist(ndd10$event, breaks = 600)
text(nrow(ndd10)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(1,2))
hist(ndd15$event, breaks = 600)
text(nrow(ndd15)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(1,2))
hist(ndd20$event, breaks = 600)
text(nrow(ndd20)/100, x = 50, y = 50, cex = 3, col = "lightgrey")

# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 20 Longer Runs NDD ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------


ndd001 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N0.01_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd01 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N0.1_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N1_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N5_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N10_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N15_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20LnDD/fitness_mortality_N20_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

# filter to get only the last 100 generations
l <- nrow(ndd001)
ndd001 <- ndd001[c((l-10000):l),]
l <- nrow(ndd01)
ndd01 <- ndd01[c((l-10000):l),]
l <- nrow(ndd1)
ndd1 <- ndd1[c((l-10000):l),]
l <- nrow(ndd5)
ndd5 <- ndd5[c((l-10000):l),]
l <- nrow(ndd10)
ndd10 <- ndd10[c((l-10000):l),]
l <- nrow(ndd15)
ndd15 <- ndd15[c((l-10000):l),]
l <- nrow(ndd20)
ndd20 <- ndd20[c((l-10000):l),]

sum(ndd001$fit)
sum(ndd20$fit)


par(mfrow = c(3,3), mar = c(1,3,3,1))
plot(ndd001$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.01", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd01$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda0.1", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)


# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 20 Longer Runs NDD LOW BOOOOOOST ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------


ndd1 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N1_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd3 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N3_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd4 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N4_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd5 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N5_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd7.5 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N7.5_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd10 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N10_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd12.5 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N12.5_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd15 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N15_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
ndd20 <- read.delim("../local/debug_pdd_ndd/expKernel/lowLBoost/fitness_mortality_N20_P0.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

# filter to get only the last 100 generations
l <- nrow(ndd1)
ndd1 <- ndd1[c((l-10000):l),]
l <- nrow(ndd3)
ndd3 <- ndd3[c((l-10000):l),]
l <- nrow(ndd4)
ndd4 <- ndd4[c((l-10000):l),]
l <- nrow(ndd5)
ndd5 <- ndd5[c((l-10000):l),]
l <- nrow(ndd7.5)
ndd7.5 <- ndd7.5[c((l-10000):l),]
l <- nrow(ndd10)
ndd10 <- ndd10[c((l-10000):l),]
l <- nrow(ndd12.5)
ndd12.5 <- ndd12.5[c((l-10000):l),]
l <- nrow(ndd15)
ndd15 <- ndd15[c((l-10000):l),]
l <- nrow(ndd20)
ndd20 <- ndd20[c((l-10000):l),]

sum(ndd001$fit)
sum(ndd20$fit)


par(mfrow = c(3,4), mar = c(1,3,3,1))
plot(ndd1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda1", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd3$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda3", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd4$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda4", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda5", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd7.5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda7.5", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda10", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd12.5$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda12.5", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd15$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda15", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(ndd20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "pdd_lambda20", ylim = c(0,1.1), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)


# ------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------FACTOR 20 Longer NDD + PDD ----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------


N1_P1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N1_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N1_P10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N1_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N1_P20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N1_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N10_P1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N10_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N10_P10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N10_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N10_P20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N10_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N20_P1 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N20_P1.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N20_P10 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N20_P10.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))
N20_P20 <- read.delim("../local/debug_pdd_ndd/expKernel/factor20NDDPDD/fitness_mortality_N20_P20.txt", col.names = c("event", "fit", "deathChance", "N_relat", "P_relat"))

# plot(ndd01$deathChance, xlab = "individuen\nüber gesamten run", ylab = "todes-chance")
# dif <- read.delim("../local/debug_pdd_ndd/expKernel/factor5/differences.txt")
# hist(dif$X0, breaks = 1000)
# range(dif$X0)

# filter to get only the last 100 generations
l <- nrow(N1_P1)
N1_P1 <- N1_P1[c((l-10000):l),]
l <- nrow(N1_P10)
N1_P10 <- N1_P10[c((l-10000):l),]
l <- nrow(N1_P20)
N1_P20 <- N1_P20[c((l-10000):l),]
l <- nrow(N10_P1)
N10_P1 <- N10_P1[c((l-10000):l),]
l <- nrow(N10_P10)
N10_P10 <- N10_P10[c((l-10000):l),]
l <- nrow(N10_P20)
N10_P20 <- N10_P20[c((l-10000):l),]
l <- nrow(N20_P1)
N20_P1 <- N20_P1[c((l-10000):l),]
l <- nrow(N20_P10)
N20_P10 <- N20_P10[c((l-10000):l),]
l <- nrow(N20_P20)
N20_P20 <- N20_P20[c((l-10000):l),]

par(mfrow = c(3,3))
plot(N1_P1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N1_P1", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N1_P10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N1_P10", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N1_P20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N1_P20", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N10_P1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N10_P1", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N10_P10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N10_P10", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N10_P20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N10_P20", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N20_P1$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N20_P1", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N20_P10$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N20_P10", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)

plot(N20_P20$fit,  xlab = "individuen\nüber gesamten run", ylab = "fitness werte", main = "N20_P20", ylim = c(0,3), col = rgb(0,0,0,.1), cex = .3)
abline( h = 1)


