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
# std dev = 6.4%
# predicted movement = 1.9%
# For normal
0.019 + 2.33*6.4
# 14.93% increase is the 1% VaR
0.019 + 3.09*6.4
# 19.8% increase is the 0.1% VaR
# What if it was a t(2) distribution?
0.019 + 6.96*6.4
# 44.6% increase
0.019 + 22.33*6.4
# 143% increase

# Do this by simulation
simreturns <- rnorm(100000, mean=0.019,
                    sd=0.064)
quantile(simreturns, 0.95)  
quantile(simreturns, 0.99)  
quantile(simreturns, 0.999)  

# VaR but not conditional volatility
# Use quantile regression to directly
# estimate
tsreg(dwti, 1)
library(quantreg)
rq(dwti ~ 1, 0.5)  
rq(dwti ~ 1, 0.05)
rq(dwti ~ 1, 0.01)
rq(dwti ~ 1, 0.001)
rq(dwti ~ 1, 0.95)
rq(dwti ~ 1, 0.99)
rq(dwti ~ 1, 0.999)
