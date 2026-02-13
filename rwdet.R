# Replicability
set.seed(2026)
n <- 200
t <- 1:n
e <- rnorm(n, mean = 0, sd = 1)

# Generate the three cases
# 1. Pure Random Walk
rw <- cumsum(e)
# 2. Random Walk with Drift (Constant = 0.5)
rw_drift <- cumsum(0.5 + e)
# 3. Random Walk with Drift (0.5) and Trend (0.05*t)
rw_trend <- cumsum(0.5 + 0.05*t + e)

# Setup 2x3 plotting grid
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# --- ROW 1: LEVELS (y_t) ---

# Plot 1: Pure RW Level
plot(rw, type = "l", main = "1. Pure RW (Level)", col = "black", ylab = "y_t")
abline(h = rw[1], lty = 3, col = "gray40") # Starting point reference

# Plot 2: RW with Drift Level
plot(rw_drift, type = "l", main = "2. RW + Drift (Level)", col = "red", ylab = "y_t")
# Note: This looks like a linear trend

# Plot 3: RW with Trend Level
plot(rw_trend, type = "l", main = "3. RW + Trend (Level)", col = "blue", ylab = "y_t")
# Note: This looks like an accelerating (quadratic) curve

# --- ROW 2: DIFFERENCES (Δy_t) ---

# Plot 4: Pure RW Diff
plot(diff(rw), type = "l", main = "4. Δ Pure RW", col = "black", ylab = "Δy_t")
abline(h = 0, col = "darkgray", lwd = 2) # Centered at zero

# Plot 5: RW with Drift Diff
plot(diff(rw_drift), type = "l", main = "5. Δ RW + Drift", col = "red", ylab = "Δy_t")
abline(h = 0.5, col = "darkgray", lwd = 2) # Centered at 0.5 (the drift)

# Plot 6: RW with Trend Diff
d_rw_trend <- diff(rw_trend)
plot(d_rw_trend, type = "l", main = "6. Δ RW + Trend", col = "blue", ylab = "Δy_t")
abline(lm(d_rw_trend ~ t[-1]), col = "darkgray", lwd = 2) # The linear trend remains

# Reset layout
par(mfrow = c(1, 1))
