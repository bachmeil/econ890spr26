library(tstools)
wti <- import.fred("wti.csv")
dubai <- import.fred("dubai.csv")
brent <- import.fred("brent.csv")
plot(wti %~% brent %~% dubai, plot.type="single")
mean(brent)
mean(window(wti, start=c(2003,1)))
mean(dubai)
start(brent)
start(wti)
start(dubai)

deviation <- wti - brent
plot(deviation)
# Do unit root test
library(urca)
summary(ur.df(deviation, selectlags="AIC"))
deviation2 <- wti - dubai
plot(deviation2)
summary(ur.df(deviation2, selectlags="AIC"))
deviation3 <- dubai - brent
plot(deviation3)
summary(ur.df(deviation3, selectlags="AIC"))

gas <- 42*import.fred("gas.csv")
plot(wti %~% gas, plot.type="single")

fit.eg <- tsreg(gas, wti)
fit.eg
fit.eg2 <- tsreg(log(gas), log(wti))
fit.eg2
z.eg <- fit.eg$resids
plot(z.eg)
library(aTSA)
ds <- gas %~% wti
eg.test <- coint.test(ds[,1], ds[,2])
