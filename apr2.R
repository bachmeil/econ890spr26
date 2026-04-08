library(tstools)
wti <- import.fred("wti.csv")
dwti <- pctChange(wti)
brent <- import.fred("brent.csv")
dbrent <- pctChange(brent)
dubai <- import.fred("dubai.csv")
ddubai <- pctChange(dubai)

# Put all in one dataset
ds <- dwti %~% dbrent %~% ddubai
start(ds)
end(ds)

# Compute principal components
pc <- princomp(ds)
names(pc)
pc$loadings
pc$loadings[,1]
pc1 <- ts(ds %*% pc$loadings[,1],
          start=start(ds),
          frequency=12)
ds[1,]
plot(pc1, type="l")
plot(pc1)
summary(tsreg(dwti, pc1))
summary(tsreg(dbrent, pc1))
summary(tsreg(ddubai, pc1))
# pc1 is "world oil price"
# Use pc1 to forecast
# Could potentially include lags of pc1

# 18 potential regressors
# 3 variables
# lags 1-6 of each variable
library(glmnet)

gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
fit.lasso <- tslasso(dgas, lags(ds, 1:6))
fit.lasso$coef

# h-step approach
fit.lasso <- tslasso(dgas, lags(ds, 3:5))
fit.lasso$coef




