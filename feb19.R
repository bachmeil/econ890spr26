library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
fit.ar1 <- tsreg(dgas, lags(dgas,1))
fit.ar1

oil <- import.fred("wti.csv")
doil <- pctChange(oil)
fit.arx <- tsreg(dgas, lags(dgas %~% doil, 1))
fit.arx

# Making forecasts with ARX
# One step
0.001569 + 0.183*last(dgas) + 0.25*last(doil)
# Compare with AR forecast
0.002295 + 0.42*last(dgas)

# Two step
0.001569 + 0.183*(-0.017) + 0.25*1
# We have no forecast of the price of oil
# You can't calculate a forecast of oil
# Would need an external forecast
# Make the oil price endogenous

library(vars)
ds <- dgas %~% doil

# Lag selection
VARselect(ds)
VARselect(ds, lag.max=13)

# Estimate the model
varfit <- VAR(ds, p=3)
varfit

# Select lag length while estimating the model
varfit <- VAR(ds, lag.max=13, ic="SC")
varfit
varfit <- VAR(ds, lag.max=13, ic="AIC")
varfit

# Can use the predict function
pred <- predict(varfit, n.ahead=6)
pred
pred$fcst$dgas[,1]

# tstools version
getVarForecasts(varfit, var="dgas")
getVarForecasts(varfit, var="dgas", n=1:12)





