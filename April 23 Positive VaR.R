x <- seq(-4, 4, length.out = 500) 

# 2. Calculate the CDF (Cumulative Distribution Function)
y <- pnorm(x)

# 3. Create the base plot
plot(x, y, type = "l", lwd = 2, col = "blue",
     main = "VaR Cutoffs - Normal Distribution",
     xlab = "Return (Standard Deviations)",
     ylab = "Cumulative Probability",
     panel.first = grid())

# 4. Define the VaR levels
levels <- c(0.95, 0.99, 0.999)
colors <- c("orange", "red", "darkred")
labels <- c("5% VaR", "1% VaR", "0.1% VaR")

# 5. Calculate cutoffs using the quantile function (qnorm)
cutoffs <- qnorm(levels)

# 6. Add vertical lines for the cutoffs
abline(v = cutoffs, col = colors, lty = 2, lwd = 2)

# 7. Add horizontal lines to show the probability mapping
#abline(h = levels, col = colors, lty = 3)

# 8. Add a legend
#legend("topleft", legend = labels, col = colors, lty = 2, lwd = 2)

# 9. Optional: Annotate the cutoff values on the x-axis
text(cutoffs, 0.1, labels = round(cutoffs, 2), pos = 2, col = colors, cex = 0.8)

