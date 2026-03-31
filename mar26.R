library(tstools)
wti <- import.fred("wti.csv")
brent <- import.fred("brent.csv")
dwti <- pctChange(wti)
dbrent <- pctChange(brent)

# OOS AR forecasts
# e is end date of the estimation sample
ar.forecast.calc <- function(e) {
  dwti.sample <- window(dwti, end=e)
  fit <- arfit(dwti.sample, 1)
  return(prediction(fit, 1))
}
ar.forecast.calc(c(2002,4))
estimation.dates <- dates(c(2014,12),
                          c(2025,12), 12)
ar.forecasts.raw <- lapply(estimation.dates,
                           ar.forecast.calc)
ar.forecasts.vector <- collect(
  ar.forecasts.raw, output="numeric")
ar.forecasts <- ts(ar.forecasts.vector,
                   start=c(2015,1),
                   frequency=12)
ar.errors <- window(dwti, start=c(2015,1)) -
  ar.forecasts
plot(ar.errors)


# OOS VAR forecasts
# e is end date of the estimation sample
library(vars)
ds <- dwti %~% dbrent
var.forecast.calc <- function(e) {
  ds.sample <- window(ds, end=e)
  fit <- VAR(ds.sample, p=1)
  return(getVarForecast(fit, 1))
}
var.forecast.calc(c(2017,4))
estimation.dates <- dates(c(2014,12),
                          c(2025,12), 12)
var.forecasts.raw <- lapply(estimation.dates,
                           var.forecast.calc)
var.forecasts.vector <- collect(
  var.forecasts.raw, output="numeric")
var.forecasts <- ts(var.forecasts.vector,
                   start=c(2015,1),
                   frequency=12)
var.errors <- window(dwti, start=c(2015,1)) -
  var.forecasts
plot(var.errors)

mean(ar.errors^2)
mean(var.errors^2)
