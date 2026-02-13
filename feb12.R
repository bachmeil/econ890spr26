library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
library(urca)
df <- ur.df(y=dgas, type="none",
            selectlags="AIC")
summary(df)
# Reject H0, don't difference the
# series

df <- ur.df(y=gas, type="none",
            selectlags="AIC")
summary(df)
# Don't reject H0
# Difference the series

df <- ur.df(y=gas, type="drift",
            selectlags="AIC")
summary(df)
# Don't reject

df <- ur.df(y=gas, type="trend",
            selectlags="AIC")
summary(df)
# Reject, so no need to difference
plot(gas)

df <- ur.df(y=gas, type="drift",
            selectlags="BIC")
summary(df)

df <- ur.df(y=gas, type="drift",
            selectlags="Fixed",
            lags=4)
summary(df)
