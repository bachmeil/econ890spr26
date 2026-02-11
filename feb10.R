For extended discussion of this program, [see this file](Feb 10 Comparisons.pdf)

library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
start(dgas)
end(dgas)

# Mean forecasts
# If forecast out-of-sample
# Use data through Dec 25 forecast
# Jan 26
mean(dgas)
# Forecast 0.4% increase in Jan 26
# But we have to use the data
# actually available
mean(window(dgas, end=c(1999,12)))
# Jan 2000 forecast 0.313%
# Actual Jan 2000
tsobs(dgas, c(2000,1))
0.0023-0.0031

mean(window(dgas, end=c(2000,1)))
# Feb 2000 forecast 0.313%
# Actual Feb 2000
tsobs(dgas, c(2000,2))
0.0523-0.0031

# Too much work to do with copy
# and paste!
# Automate it
# Specify the end date of the sample
# used for estimation
estimation.dates <- dates(c(1999,12),
                          c(2025,11), 12)
estimation.dates

# Now create a function that makes the
# forecast given the end date of the sample
mean.forecast.calc <- function(e) {
  mu <- mean(window(dgas, end=e))
  return(mu)
}
mean.forecast.calc(c(2020,1))
mean.forecasts.raw <- lapply(estimation.dates,
                             mean.forecast.calc)
mean.forecasts.raw
# Now have 312 forecasts for Jan 2000-Dec 2025
# Convert to a vector
mean.forecasts.vector <- collect(
  mean.forecasts.raw, output="numeric")
mean.forecasts.vector
mean.forecasts <- ts(mean.forecasts.vector,
                     start=c(2000,1),
                     frequency=12)
plot(mean.forecasts)
# Now calculate the MSE of the mean
# model
mean.errors <- window(dgas,start=c(2000,1)) -
  mean.forecasts
plot(mean.errors)
mean(mean.errors^2)


# Now do the same for ARMA(1,1)
arma.forecast.calc <- function(e) {
  dgas.sample <- window(dgas, end=e)
  fit <- armafit(dgas.sample, 1, 1)
  return(prediction(fit, 1))
}
arma.forecast.calc(c(2000,1))

# Make all the forecasts for Jan 2000-Dec 2025
arma.forecasts.raw <- lapply(estimation.dates,
                             arma.forecast.calc)
# Now have 312 forecasts for Jan 2000-Dec 2025
# Convert to a vector
arma.forecasts.vector <- collect(
  arma.forecasts.raw, output="numeric")
arma.forecasts <- ts(arma.forecasts.vector,
                     start=c(2000,1),
                     frequency=12)
plot(arma.forecasts)
arma.errors <- window(dgas,start=c(2000,1)) -
  arma.forecasts
mean(arma.errors^2)

library(urca)
df <- ur.df(y=dgas, type="none",
            selectlags="AIC")
summary(df)
df <- ur.df(y=gas, type="none",
            selectlags="AIC")
summary(df)
