#==========================================================
# purpose: estimation of in-sample VaR using various GARCH
#   models
# These examples are taken from the G@RCH 6.1 manual (Section 
#   6.1: VaR models; http://www.timberlake.co.uk/slaurent/G@RCH/default.htm)
# The data is distributed with the console version of the G@RCH 
#   6.1 software
# author: TC
# created: 11th February 2015
# revised:
# comments:
# TODO:
#==========================================================

library(xlsx)
library(xts)
library(rugarch)

# read in the data
dfN = read.xlsx("Data//nasdaq.xls", sheetIndex = 1)
xN = xts(dfN$Nasdaq, order.by = dfN$Date)            

# fit an ARMA(2, 0)-APARCH(1, 1) Normal model
gN1 = ugarchspec(variance.model = list(model = "apARCH",
                                 garchOrder = c(1, 1)), 
           mean.model = list(armaOrder = c(2, 0), 
                             include.mean = TRUE),
           distribution.model = "norm")
gfN1 = ugarchfit(gN1, data = xN["/2000-01-01"])

# fit an ARMA(2, 0)-APARCH(1, 1) skewed-Student model
gN2 = ugarchspec(variance.model = list(model = "apARCH",
                                       garchOrder = c(1, 1)), 
                 mean.model = list(armaOrder = c(2, 0), 
                                   include.mean = TRUE),
                 distribution.model = "sstd")
gfN2 = ugarchfit(gN2, data = xN["/2000-01-01"])
coef(gfN2)

ugarchroll(gN2, data = xN, n.ahead = 1, 
           calculate.VaR = TRUE, VaR.alpha = c(0.05, 0.95))