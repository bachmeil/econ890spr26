library(tsgarch)
library(xts)
library(tstools)
wti <- import.fred("wti.csv")
dwti <- pctChange(wti)
fit.arma <- armafit(dwti, ar=1, ma=1)
res <- fit.arma$res
plot(res)

spec <- garch_modelspec(
  xts(res, as.Date(res)),
  model="garch",
  order=c(1,1)
)
fit.garch <- estimate(spec)
predict(fit.garch, h=1)
predictions(fit.arma, 1)

# Prediction of dwti is +0.019
# Predicted volatility is 0.064
qnorm(0.05)
qnorm(0.01)
qnorm(0.001)

0.019 + 3.09*0.064
0.019 + 2.33*0.064
0.019 + 1.645*0.064
