# Correlation of independent variables
set.seed(100)
y <- rnorm(500)
x <- rnorm(500)
cor(y, x)

set.seed(100)
y <- rnorm(100)
x <- rnorm(100)
cor(y, x)

set.seed(100)
y <- rnorm(5)
x <- rnorm(5)
cor(y, x)

set.seed(100)
y <- rnorm(2)
x <- rnorm(2)
cor(y, x)
y - mean(y)
# Correlation is meaningless in a small
# sample

set.seed(100)
output1 <- replicate(1000, {
  y <- rnorm(500)
  x <- rnorm(500)
  cor(y, x)
})
mean(output1)
plot(density(output1))

set.seed(100)
output2 <- replicate(1000, {
  y <- rnorm(100)
  x <- rnorm(100)
  cor(y, x)
})
mean(output2)
plot(density(output2))

set.seed(100)
output3 <- replicate(1000, {
  y <- rnorm(5)
  x <- rnorm(5)
  cor(y, x)
})
mean(output3)
plot(density(output3))

z <- 1:4
rowSums(embed(z,2))

set.seed(100)
# Introduce dependence
# 50% shared variance from t to t+1
output4 <- replicate(1000, {
  y <- ts(rnorm(501))
  y2 <- rowSums(embed(y,2))
  x <- ts(rnorm(501))
  x2 <- rowSums(embed(x,2))
  cor(y2, x2)
})
mean(output4)
plot(density(output4))

# Add five errors together
set.seed(100)
# Add more dependence
# 5 common error terms
# But still 500 observations
output5 <- replicate(1000, {
  y <- ts(rnorm(505))
  y2 <- rowSums(embed(y,6))
  x <- ts(rnorm(505))
  x2 <- rowSums(embed(x,6))
  cor(y2, x2)
})
mean(output5)
plot(density(output5))
# Dependence in y and x is the same
# as a smaller sample size

# Add 36 errors together
set.seed(100)
# Add more dependence
# 5 common error terms
# But still 500 observations
output6 <- replicate(1000, {
  y <- ts(rnorm(535))
  y2 <- rowSums(embed(y,36))
  x <- ts(rnorm(535))
  x2 <- rowSums(embed(x,36))
  cor(y2, x2)
})
mean(output6)
plot(density(output6))
# Dependence in y and x is the same
# as a smaller sample size

# Add 250 errors together
set.seed(100)
# Add more dependence
# 5 common error terms
# But still 500 observations
output7 <- replicate(1000, {
  y <- ts(rnorm(749))
  y2 <- rowSums(embed(y,250))
  x <- ts(rnorm(749))
  x2 <- rowSums(embed(x,250))
  cor(y2, x2)
})
mean(output7)
plot(density(output7))
# Dependence in y and x is the same
# as a smaller sample size

# A large dataset with strong dependence
# is meaningless for forecasting.
# It will look like there's a relationship
# but it's *spurious*.