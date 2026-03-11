library(tstools)
dubai <- import.fred("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/dubai.csv")
brent <- import.fred("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/brent.csv")

# Question 1
plot(dubai)
plot(brent)
# Based on the plots, the pure random walk model is the appropriate
# one for the level.
library(urca)
summary(ur.df(y=dubai, type="none", selectlags="AIC"))
summary(ur.df(y=brent, type="none", selectlags="AIC"))
# Both fail to reject the null hypothesis
# Conclude they are nonstationary

# Question 2
# We'll use the percentage change
ddubai <- pctChange(dubai)
dbrent <- pctChange(brent)
ds <- ddubai %~% dbrent
library(vars)
varfit <- VAR(ds, lag.max=13, ic="SC")
varfit

# Question 3
# Dubai T+1
0.004145496 + 0.898189256*last(ddubai) - 0.582828939*last(dbrent)
# Brent T+1
0.00397197 + 0.87182497*last(ddubai) - 0.55162428*last(dbrent)
# Dubai T+2
0.004145496 + 0.898189256*(-0.009050083) - 0.582828939*(-0.00820153)
# Brent T+2
0.00397197 + 0.87182497*(-0.009050083) - 0.55162428*(-0.00820153)

# Question 4
# Arbitrarily putting dubai on the left
fit.eg <- tsreg(dubai, brent)
fit.eg
z <- fit.eg$resids
library(aTSA)
# Match dates
levels <- dubai %~% brent
eg.test <- coint.test(levels[,1], levels[,2])
# p-value is 0.0631 => Don't reject H0 => Not cointegrated
# VEC model
fit.1 <- tsreg(ddubai, lags(ddubai %~% dbrent %~% z, 1))
fit.1
0.004162 + 0.957311*last(ddubai) - 0.646892*last(dbrent) - 0.002175*last(z)
fit.2 <- tsreg(ddubai, lags(ddubai %~% dbrent %~% z, 2))
fit.2
0.006272 + 0.833396*last(ddubai) - 0.804302*last(dbrent) - 0.005726*last(z)
fit.3 <- tsreg(ddubai, lags(ddubai %~% dbrent %~% z, 3))
fit.3
0.008041 - 0.183005*last(ddubai) + 0.072197*last(dbrent) - 0.007182*last(z)
