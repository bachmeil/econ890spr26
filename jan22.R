library(tstools)
# import.fred for FRED data
# time series: frequency and date
# Use this version if the file is saved on your computer
# You may need to add a path if it's not in the working directory
# wti <- import.fred("wti.csv")
# You can use a URL and it will be downloaded from the web
# It's okay to download the file a few times, but if you download 
# it many times, Github might think you're causing trouble and block you
wti <- import.fred("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/wti.csv")
wti
gas <- import.fred("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/gas.csv")
# gas <- import.fred("gas.csv")
co2.us <- import.fred("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/co2.csv")
#co2.us <- import.fred("co2.csv")

# Plot the series
plot(wti)
plot(gas)
plot(co2.us)

# Other data (more general)
# This is a general approach that will work to load any CSV dataset
# Other text formats will work too if you pass the right arguments
co2.china.raw <- read.csv("https://raw.githubusercontent.com/bachmeil/econ890spr26/refs/heads/main/china-co2.csv",
                          header=TRUE)

# co2.china.raw <- read.csv("china-co2.csv",
#                          header=TRUE)
co2.china <- ts(co2.china.raw[,4],
                frequency=1,
                start=1907)
plot(co2.china)

gas
# Using the bad function!
lag(gas,1)

# Using the lags function
lags(gas, 1)
lags(gas, 1:2)
lags(gas, 0:2)

# Lead
lags(gas, -1)

start(gas)
end(gas)
frequency(gas)
# One observation
tsobs(gas, c(2000,6))
# Many observations
window(gas, start=c(2000,1), 
       end=c(2000,12))
window(gas)
window(gas, start=c(2020,1))
window(gas, end=c(1999,12))

# Title
plot(gas, main="Monthly US Gasoline Price")

# Remove axis labels
plot(gas, main="Monthly US Gasoline Price",
     xlab="", ylab="")

# Line width
plot(gas, lwd=0.2)
plot(gas, lwd=2)

# Setting the range
plot(gas, ylim=c(0, 10))
plot(gas, xlim=c(1950,2025))
# ts plot is x-y plot where x is time
# whole numbers are the year

# Multiple plots
# 1 row with two plots
par(mfrow=c(1,2))
plot(gas)
plot(wti)
par(mfrow=c(1,1))


