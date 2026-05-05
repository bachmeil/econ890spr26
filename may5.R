library(tstools)
yen <- import.fred("yen.csv")
dyen <- pctChange(yen)
plot(dyen)
rea <- import.fred("rea.csv")
wti <- import.fred("wti.csv")
dwti <- pctChange(wti)
fit.dwti <- tsreg(dwti, lags(rea,0:3))
res <- residuals(fit.dwti)
plot(res)

# Predict the yen
fit.yen <- tsreg(dyen, lags(dyen %~% res, 1))
summary(fit.yen)
fit.yen <- tsreg(dyen, lags(dyen, 1) %~%
                   lags(res, 0:1))
summary(fit.yen)
ccf(dyen, res)
