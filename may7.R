# Linear model
s1 <- 0
s2 <- 0
s3 <- 0
# Start out with y = 1

y1 <- 0.5*1 + s1
y1
y2 <- 0.5*y1 + s2
y2
y3 <- 0.5*y2 + s3
y3

# Different set of shocks
s1 <- 0.4
s2 <- 0.1
s3 <- 0.1
# Start out with y = 1

y1 <- 0.5*1 + s1
y1
y2 <- 0.5*y1 + s2
y2
y3 <- 0.5*y2 + s3
y3

# Double the shocks
s1 <- 0.8
s2 <- 0.2
s3 <- 0.2
# Start out with y = 1

y1 <- 0.5*1 + s1
y1
y2 <- 0.5*y1 + s2
y2
y3 <- 0.5*y2 + s3
y3

# Threshold model
s1 <- 0
s2 <- 0
s3 <- 0
# Start out with y = 0

y1 <- 0.5*0 + s1
y1
y2 <- 0.5*0 + s2
y2
y3 <- 0.5*0 + s3
y3

s1 <- 0.4
s2 <- 0.1
s3 <- 0.1
# Start out with y = 0

y1 <- 0.5*0 + s1
y1
y2 <- 0.5*0.4 + s2
y2
y3 <- 0.5*0.3 + s3
y3

s1 <- 0.8
s2 <- 0.2
s3 <- 0.2
# Start out with y = 0

y1 <- 0.5*0 + s1
y1
y2 <- -0.5*0.8 + s2
y2
y3 <- 0.5*-0.2 + s3
y3

library(tstools)
gas <- import.fred("gas.csv")
dgas <- pctChange(gas)
dgas.pos <- (dgas > 0)*dgas
plot(dgas.pos)
tsreg(dgas, lags(dgas %~% dgas.pos, 3))

# Forecast price of gas for May 2026
# Nonlinear AR(2)
# Input last two values of dgas
inputs <- c(0.128, 0.251)

# Convert those two values into a forecast
# for May 2026

# First hidden layer
# 3 neurons
weights.hidden <- matrix(0.0, nrow=3, ncol=3)
weights.hidden[1,] <- c(0.1, -0.5, 0)
weights.hidden[2,] <- c(-0.5, -0.8, 1.2)
weights.hidden[3,] <- c(0.0, 0.1, 0.4)
weights.hidden
# Calculate neuron values
z <- weights.hidden %*% c(1, inputs)
z
# Activations
# This is the nonlinearity
activated.outputs <- pmax(0, z)
activated.outputs

# For one layer model, that's it
# Make forecast from the activations
# Use vector of output weights
output.weights <- c(0.7, -0.3, 1.2)
final.forecast <- 
  sum(activated.outputs * output.weights)
final.forecast
# Forecast is that May 2026 gas price
# increase is 16.1%
# but that is given by the neurons and
# the output weights

# How are the output weights and neurons chosen?
# Use the ones that match the data best
# Lowest MSE of pseudo-OOS forecasts

# 1 hidden layer
# Multiple layers
# Second layer
# Repeat the process with 3 activations
# as the inputs
# Convert to three new activations
# Two-layer model


