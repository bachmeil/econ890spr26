library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
oil <- import.fred("wti.csv")
doil <- pctChange(oil)

# Make some forecasts manually
library(vars)
ds <- dgas %~% doil

# Use a VAR(2)
varfit <- VAR(ds, p=2)
varfit
tail(ds,2)
# Make T+1 forecasts
# Gas forecasts
0.002 + 0.27*(-0.055) + 0.23*(-0.035) -
  0.12*(-0.003) - 0.017*(-0.014)
# dgas(T+1)
# Now doil(T+1)
0.005 - 0.21*(-0.055) + 0.34*(-0.035) +
  0.15*(-0.003) - 0.104*(-0.014)

# dgas(T+2)
0.002 + 0.27*(-0.02) + 0.23*(0.006) -
  0.12*(-0.055) - 0.017*(-0.034)

# Now recover the forecast of the
# level
last(gas)
# gas(T+1)
3.05*(1-0.02)
# gas(T+2)
3.05*(1-0.02)*(1+0.005)

# Alternative approach
# h-step projection approach
# Forecast gas price change in six months
tsreg(dgas, lags(dgas %~% doil, 6:7))
0.005 - 0.22*(-0.055) - 0.12*(-0.003) -
  0.008*(-0.035) 
+ 0.122*(-0.014)
# Forecast of dgas(T+6)
# 0.018

# Evaluate VAR forecasts out of sample
ds <- dgas %~% doil

var.forecast.calc <- function(e) {
  ds.sample <- window(ds, end=e)
  varfit <- VAR(ds.sample, lag.max=13,
                ic="SC")
  return(getVarForecast(varfit, var="dgas",
                        n=3))
}
var.forecast.calc(c(1999,10))
estimation.dates <- dates(c(1999,10),
                          c(2025,9), 12)
var.forecasts.raw <- lapply(estimation.dates,
                            var.forecast.calc)
var.forecasts.vector <- collect(
  var.forecasts.raw, output="numeric")
var.forecasts <- ts(var.forecasts.vector,
                    start=c(2000,1),
                    frequency=12)
plot(var.forecasts)
var.forecasts
var.error <- dgas - var.forecasts
plot(var.error)
# Is there bias?
mean(var.error)
# Not much if any
# How large on average?
sd(var.error)
# Efficiency
cor(var.error %~% lags(doil,3))
