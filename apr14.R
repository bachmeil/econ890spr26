# Constant volatility model
library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
arma.dgas <- armafit(dgas, 1, 1)
dgas.error <- residuals(arma.dgas)
volatility.constant <- sd(dgas.error)
volatility.constant

# Make prediction
prediction(arma.dgas, 1)
# 95% interval for actual value
-0.028 + 1.96*volatility.constant
-0.028 - 1.96*volatility.constant

# Time-varying volatility
set.seed(100)
ysim1 <- rnorm(250)
ysim2 <- rnorm(250, sd=2.0)
ysim3 <- rnorm(250, sd=0.5)
ysim4 <- rnorm(250, sd=1.1)
ysim <- ts(c(ysim1, ysim2, ysim3, ysim4))
plot(ysim)
plot(ts(ysim1))

arma.dgas <- armafit(dgas, 1, 1)
e <- residuals(arma.dgas)
plot(e)
plot(e^2)

# Primitive volatility prediction model
e2 <- e^2
e2.ar <- arfit(e2, 3)
sqrt(predictions(e2.ar, 12))
volatility.residual <- residuals(e2.ar)
sd(volatility.residual)
sd(e2)
plot(volatility.residual)

library(fGarch)
fit <- garchFit(~ arma(2,1) + garch(1,1), 
                data=dgas)

summary(fit)
predict(fit, n.ahead=12)
