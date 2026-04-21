library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
arma.dgas <- armafit(dgas, 1, 1)
dgas.error <- residuals(arma.dgas)
e2 <- dgas.error^2
plot(e2)

# ARCH LM test
arch.fit <- tsreg(e2, lags(e2, 1:3))
summary(arch.fit)
length(e2)
599*0.1067
# Calculate the p-value
# Evaluate the CDF of the chi-sq
# with 3 df at 63.9
1-pchisq(63.9133, 3)
# Reject H0
# Engle (2001 JEP)

# The tsgarch package
fit.mean <- armafit(dgas, 2, 1)
e <- residuals(fit.mean)
plot(e)
library(xts)
index <- as.Date(1:length(e))
index
e.xts <- xts(e, index)
plot(e.xts)

# Provide a specification
library(tsgarch)
spec <- garch_modelspec(
  y = e.xts,
  model = "garch",
  order = c(1, 1),
  constant = TRUE,
  distribution = "norm"
)
fit <- estimate(spec)
summary(fit)

# Prediction of the mean
fitted(fit)

# Fitted volatility
sigma(fit)
plot(sigma(fit))

# Predicted volatility
predict(fit, h=6)

# Plot predicted volatility
plot(predict(fit, h=6)$sigma)

set.seed(100)
x1 <- rnorm(100000)
mean(abs(x1) > 2.5)

x2 <- rt(100000, 30)
mean(abs(x2) > 2.5)

x3 <- rt(100000, 10)
mean(abs(x3) > 2.5)

x4 <- rt(100000, 2)
mean(abs(x4) > 2.5)

spec <- garch_modelspec(
  y = e.xts,
  model = "garch",
  order = c(1, 1),
  constant = TRUE,
  distribution = "std"
)
fit <- estimate(spec)
summary(fit)
predict(fit, h=6)

# ged: Generalized error dist
# gh: Generalized hyperbolic
spec <- garch_modelspec(
  y = e.xts,
  model = "garch",
  order = c(2, 1),
  constant = TRUE,
  distribution = "std"
)
fit <- estimate(spec)

