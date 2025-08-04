# demo how the DD specificity is implemented

vH <- .01
vL <- 0.001
dif <- seq(0, 1, by = 0.001)

amp_high <- exp(-vH * 100)
amp_low <- exp(-vL * 100 )

varHigh <- amp_high * exp(-0.5 * (dif / vH)^2)
varLow <- amp_low * exp(-0.5 * (dif / vL)^2)

par(mfrow = c(1,2))
plot(dif, varHigh, type = "l", main = paste("High Var, amp =", round(amp_high, 2)), ylim = c(0,1))
plot(dif, varLow, type = "l", main = paste("Low Var, amp =", round(amp_low, 2)), ylim = c(0,1))

