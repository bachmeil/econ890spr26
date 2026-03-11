library(tstools)
wti <- import.fred("wti.csv")
brent <- import.fred("brent.csv")
library(urca)
vec <- ca.jo(wti %~% brent)
vec
summary(vec)
vec <- ca.jo(brent %~% wti)
summary(vec)

vec <- ca.jo(wti %~% brent,
             ecdet="const")
summary(vec)

vec <- ca.jo(wti %~% brent,
             ecdet="trend")
summary(vec)

vec <- ca.jo(wti %~% brent,
             type="trace")
summary(vec)




