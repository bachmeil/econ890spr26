library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
length(dgas)
frequency(dgas)
e0 <- 0

# y1 = b0 + b1*e0 + e1
# e1 = y1 - b0 - b1*e0
calc.error <- function(val, err) {
  return(val - b0 - b1*err)
}
b0 <- 0
b1 <- 0.8
e1 <- calc.error(dgas[1], 0)
e2 <- calc.error(dgas[2], e1)
e3 <- calc.error(dgas[3], e2)
e4 <- calc.error(dgas[4], e3)
e5 <- calc.error(dgas[5], e4)
e1
e2
e3
e4
e5
e1^2 + e2^2 + e3^2 + e4^2 + e5^2
# SSE 0.000194

# Now estimate an MA(1)
ma1 <- mafit(dgas, 1)
ma1
last(residuals(ma1))
# Make forecast for Jan 2026
0.0039 + 0.487*last(residuals(ma1))
# -0.027
# R built-in function
predict(ma1, 1)
predict(ma1, 12)

# tstools function
predictions(ma1, 1)
predictions(ma1, 12)
prediction(ma1, 1)
prediction(ma1, 12)

# MA(2)
ma2 <- mafit(dgas, 2)
ma2
prediction(ma2, 1)
# What is the best lag length for
# the approximate Wold Rep?

ma3 <- mafit(dgas, 3)

# If using the AIC
AIC(ma1)
AIC(ma2)
AIC(ma3)
# AIC chooses the MA(3)
# SIC selection
BIC(ma1)
BIC(ma2)
BIC(ma3)
# SIC chooses the MA(1)