library(tstools)
wti <- import.fred("wti.csv")
brent <- import.fred("brent.csv")
dwti <- pctChange(wti)
dbrent <- pctChange(brent)

library(urca)
vec <- ca.jo(wti %~% brent)
summary(vec)

# Calculate the error correction term
z <- wti - 0.784*brent
plot(z)

# Do forecasts one, two and three steps
# ahead for dwti
end(wti)
end(brent)
fit.wti1 <- tsreg(dwti, 
      lags(dwti %~% dbrent %~% z, 1))
fit.wti1
last(dwti)
last(dbrent)
last(z)
# One-step forecast
0.052 - 0.042*0.036 + 0.38*0.045 -
  0.004*9.398
# February prediction is that WTI rises
# about 3%

fit.wti2 <- tsreg(dwti, 
  lags(dwti %~% dbrent %~% z, 2))
fit.wti2
# Two-step forecast
0.059 - 0.175*0.036 + 0.19*0.045 -
  0.004*9.398
# March prediction: rise 2.4%

fit.wti3 <- tsreg(dwti, 
  lags(dwti %~% dbrent %~% z, 3))
fit.wti3
# Three-step forecast
0.056 + 0.001*0.036 - 0.12*0.045 -
  0.004*9.398
# April prediction: rise 1.3%

# In-sample Granger causality test
# Does Brent GC WTI?
# Joint test that all coefficients on
# Brent terms are zero
fit <- tsreg(dwti,
  lags(dwti %~% dbrent, 1:3))
fit
testzero(fit, 5:7)
# Reject the null hypothesis
# Conclude Brent GC WTI





