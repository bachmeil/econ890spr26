library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
ar1 <- arfit(dgas, 1)
ar1
print(ar1)
names(ar1)
ma4 <- mafit(dgas, 4)
ma4
arma22 <- armafit(dgas, 2, 2)
arma22
predictions(ar1, 12)
plot(predictions(ar1, 12))
prediction(ar1, 12)
# Lag selection
arma.select(dgas, 3, 3)
arma.select(dgas, 2:4, 5)
armafit(dgas, 3, 3, auto=TRUE)
armafit(dgas, 3, 3, auto=list("allowdrift"=TRUE))
# Trends, seasonal dummies, etc.

# arima is built into R
arima(dgas, order=c(1,0,1))
# Issues with arima
# Intercept is confusing
# Differencing
# Documentation