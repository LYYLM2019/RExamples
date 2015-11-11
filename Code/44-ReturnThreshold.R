library(quantmod)
getSymbols("AAPL")
prices = AAPL[, "AAPL.Close"]
returns = diff(log(prices))

countThresh = function(returns, threshold) sum(returns < threshold, na.rm = TRUE)
countThresh(returns, 0.01)
