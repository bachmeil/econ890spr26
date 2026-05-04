# Work with backtesting data from last time
# Compare the volatility of the portfolios
sd1 <- sd(sp500.excess)
sd2 <- sd(portfolio2.excess)
sd1
sd2

# Sharpe ratio
# Asks whether the excess return you get per unit of volatility
# is higher or lower than that of another portfolio
# Higher volatility normally implies you should expect a higher
# return, so you have to compute the excess return per unit of volatility
mean(sp500.excess)/sd1
mean(portfolio2.excess)/sd2
# Can also use the concept of VaR
# See the 95th percentile of bad outcomes as a way to compare
# the two portfolios
# Does including energy in the portfolio provide protection against
# extreme bad outcomes?
quantile(sp500.excess, probs=0.05)
quantile(portfolio2.excess, probs=0.05)

# Work with EU ETS carbon price data
library(tstools)
eu.ets.raw <- read.csv("carbon-price-eu-data.csv",
                       header=FALSE, skip=1)
eu.ets <- ts(eu.ets.raw, frequency=1)
plot(eu.ets)

# Estimate a volatility model for the carbon price
# This captures the uncertainty faced by firms that need
# to purchase these permits in order to produce
deu.ets <- pctChange(eu.ets)
library(fGarch)
fit.garch <- garchFit(~ arma(1,1) +
                        garch(1,1),
                      data=deu.ets)
predict(fit.garch, n.ahead=10)

# Can also assess the risk to firms by looking at the 95th percentile
# of price increases over a period of time
# 5% VaR 
qnorm(0.95)
0.019*1.645

# Have there been bubbles in the EU carbon market?
library(exuber)
library(exuberdata)
adf.recursive <- radf(eu.ets,
                      minw=30,
                      lag=1)
length(eu.ets)
# This is needed because critical values have not been tabulated for
# 2710 observations with minw=30.
# This is how you should do the test for bubbles if you're getting an
# error message saying there are no critical values available.
# This is the part where my laptop died in class.
# You probably need to have enough RAM available to do the simulation.
mdist <- radf_mc_cv(n=2710,
                    minw=30,
                    nrep=1000)
autoplot(adf.recursive, cv=mdist)
