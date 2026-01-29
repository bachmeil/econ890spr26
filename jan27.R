library(tstools)
gas <- import.fred("gas.csv")
plot(gas)
# Averaging
mean(gas)
# $1.92/gallon in the future
# Recent averaging
mean(window(gas, start=c(2010,1)))
# $3.06/gallon is the forecast
# Seasonal averaging
# January forecast
# Average of previous Jan values
mean(extract.month(gas, 1))
recent.gas <- window(gas, start=c(2010,1))
mean(recent.gas)
# Random walk
# Most recent value
last(gas)
# Forecast is $3.05/gallon
# Seasonal random walk
tsobs(gas, c(2025,1))
# $3.21/gallon
# Constant drift
dgas <- pctChange(gas,1)
plot(dgas)
mean(dgas)
# Rises 0.4 percent
last(gas)
# $3.054
# Seasonal drift
mean(extract.month(dgas,1))
# Multiple horizons
# Constant drift
# Forecast five years from now
# 0.4*60 = 24%
3.05*1.24

# Looking at the random walk model
# in the past
start(gas)
# First forecast for Feb 1976
e <- gas - lags(gas,1)
plot(e)
mean(e)
# This model systematically underpredicts
# Should make a correction
# Bias
# How bad are the errors?
sd(e)
last(gas)
# Assume normality
3.05 + 1.96*0.127
3.05 - 1.96*0.127

# What about 12 month predictions?
e12 <- gas - lags(gas,12)
mean(e12)
sd(e12)
3.05 + 1.96*0.435
3.05 - 1.96*0.435
# Don't learn anything from this forecast