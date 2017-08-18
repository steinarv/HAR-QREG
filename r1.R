library(quantreg) # Quantile regression
library(GAS) # Backtesting and stock data
library(data.table) # Organising data


data("StockIndices")


head(StockIndices)
data.table(StockIndices)

y <- StockIndices[, "DAX"]
n <- length(y)

# Make data frame with explanatory variabels
df1 <- data.frame(y = y,
                  sigma1 = c( NA, sqrt(y^2)[-n] ), # Square root of yesterdays squared return
                  sigma5 = c( NA, sqrt(filter(y^2, rep(1/5, 5), sides = 1))[-n] ), # Square root of past 5 days average squared return
                  sigma20 = c( NA, sqrt(filter(y^2, rep(1/20, 20), sides = 1))[-n] ) # Square root of past 20 days average squared return
)

df1 <- na.omit(df1)
N <- nrow(df1)
# Rolling window one-day-ahead predictions with window of 1000 observations
win_size <- 1000 # Window size
pred0 <- numeric(N - win_size) # Historical simulation (static quantiles)
pred1 <- numeric(N - win_size) # HAR_QREG model
alpha <- 0.05 # Quantile to predict

for(r in win_size:(N -1)){
  rqfit <- rq(y ~ sigma1 + sigma5 + sigma20, data = df1[(r - win_size + 1):r, ], tau = alpha)
  
  pred0[r - win_size + 1] <- quantile(df1$y[(r - win_size + 1):r], probs = alpha)
  pred1[r - win_size + 1] <- predict(rqfit, df1[r + 1, ])
}

# Backtesting
btest0 <- BacktestVaR(data = df1$y[(win_size + 1):N], VaR = pred0, alpha = alpha)
btest1 <- BacktestVaR(data = df1$y[(win_size + 1):N], VaR = pred1, alpha = alpha)

# A grafical presentation of P-values for Conditional Covareage test and Dynamic Quantile test
pval0 <- c("CC" = unname(btest0$LRcc[2]), "DQ" = btest0$DQ$pvalue)
pval1 <- c("CC" = unname(btest1$LRcc[2]), "DQ" = btest1$DQ$pvalue)

barplot(c(pval0, pval1), ylim = c(0, 1), col = c(2,2,3,3), main = "P-values")
abline(h = 0.05)
legend("topleft", legend = c("Historical Simulation", "HAR QREG"), text.col = c(2,3),
       box.lty = 0)
