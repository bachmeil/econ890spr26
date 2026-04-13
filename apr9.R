# Probability price goes above 120
# Distribution of price change between now
# and 2 months in the future
1 - pnorm(10, sd=1)
1 - pnorm(10, sd=10)
1 - pnorm(10, sd=30)
1 - pnorm(15, sd=30)
1 - pnorm(20, sd=30)
# We need to forecast volatility
# Why not standard deviation from historical
# data?
# Time-varying volatility
# Prediction of the variance of the
# forecast error