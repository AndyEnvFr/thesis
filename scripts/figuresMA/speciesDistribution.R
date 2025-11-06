par(mfrow = c(1,2), mar=c(1,1,1,1))
image(runsRaw$`P1-L5-C1_N1-L20-C5`$Output$`80001`$specMat, col = rainbow(1000, rev = TRUE), xaxt = "n", yaxt = "n")
image(runsRaw$`P1-L5-C1_N0.2-L1-C1`$Output$`80001`$specMat, col = rainbow(1000, rev = TRUE), , xaxt = "n", yaxt = "n")

par(mfrow = c(1,2), mar=c(1,1,1,1))
image(runsRaw$`P1-L5-C1_N1-L20-C5`$Output$`80001`$specMat, col = rainbow(30, rev = FALSE), xaxt = "n", yaxt = "n")
image(runsRaw$`P1-L5-C1_N0.2-L1-C1`$Output$`80001`$specMat, col = rainbow(30, rev = FALSE), , xaxt = "n", yaxt = "n")


par(mfrow = c(1,2), mar=c(1,1,1,1))
image(runsRaw$`P1-L5-C1_N1-L20-C5`$Output$`80001`$specMat, col = rainbow(30, rev = FALSE), xaxt = "n", yaxt = "n")
image(runsRaw$`P1-L5-C1_N0.2-L1-C1`$Output$`80001`$specMat, col = rainbow(30, rev = TRUE), , xaxt = "n", yaxt = "n")

