library(tstools)
gas <- import.fred("gas.csv")
plot(gas)
trend <- make.trend(gas)
trend
linear.trend <- tsreg(gas, trend)
linear.trend
0.43+601*0.005
# We're not using all information
# We know the last observation
last(gas)
tsreg(gas, trend, start=c(2010,1))
trend3 <- make.trend(gas, 3)
trend3
fit3 <- tsreg(gas, trend3)
plot(fit3$fitted)

dum <- seasonal.dummy(gas)
dum
# Work with percentage change
dgas <- pctChange(gas)
plot(dgas)
cor(lags(dgas,1:4))
acf(dgas)
# Estimate an AR(1) model
# Autoregressive model
# 1 lag
tsreg(dgas, lags(dgas,1))
# AR(3)
# 3 lags
tsreg(dgas, lags(dgas,1:3))
# Turn it into a forecasting
# model
# dgas(t) = 0.0028 +
#  0.51*dgas(t-1) - 0.226*dgas(t-2)
#  + 0.001*dgas(t-3)
last(dgas,3)
0.0028 + 0.51*(-0.055) -
  0.226*(-0.0034) + 0.001*(-0.03)
# Predicting gas prices will fall
# 2.5% from December to this month