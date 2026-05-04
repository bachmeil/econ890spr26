# Testing for a bubble in the monthly WTI data
library(tstools)
library(exuber)
wti <- import.fred("wti.csv")
# radf is the recursive ADF test used for testing for explosive behavior
?radf
adf.recursive <- radf(wti, lag=1)
adf.recursive
summary(adf.recursive)
library(exuberdata)
# Run this only once if the autoplot function doesn't work
# You should not install the same library multiple times
# install_exuberdata()
diagnostics(adf.recursive)
# This is the informative plot
# It shows the recursive test statistics and valid critical values
# at each point in time
autoplot(adf.recursive)
plot(wti)


# Simple backtesting exercise
# 100% S&P 500
# 80% S&P 500 20% WTI
sp500 <- import.tsformat("sp500.csv")
plot(sp500)
dwti <- pctChange(wti)
portfolio2 <- 0.8*sp500 + 0.2*dwti
plot(portfolio2)
mean(sp500)
12*mean(sp500)
# Excess return
tbill <- import.fred("tbill3mo.csv")
sp500.excess <- 12*sp500 - tbill/100
plot(sp500.excess)
mean(sp500.excess)
portfolio2.excess <- 12*portfolio2 - tbill/100
mean(portfolio2.excess)
