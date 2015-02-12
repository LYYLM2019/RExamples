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
library(skewt)
library(reshape2)
library(ggplot2)

# read in the data
dfN = read.xlsx("Data//nasdaq.xls", sheetIndex = 1)
xN = xts(dfN$Nasdaq, order.by = dfN$Date)            
plot(xN)

#================================================
# compute the model fit
#================================================
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

# fit an ARMA(2, 0)-APARCH(1, 1) Student-t model
gN3 = ugarchspec(variance.model = list(model = "apARCH",
                                       garchOrder = c(1, 1)), 
                 mean.model = list(armaOrder = c(2, 0), 
                                   include.mean = TRUE),
                 distribution.model = "std")
gfN3 = ugarchfit(gN3, data = xN["/2000-01-01"])

#================================================
# compute the VaR
#================================================
getVaR = function(objGarchFit, vQ) {
  # get the distribution
  sDistrib = objGarchFit@model$modeldesc$distribution
  # get the conditional mean
  vCondMean = fitted(objGarchFit)
  # get the conditional s.d.
  vCondSD = sigma(objGarchFit)
  
  # pre-assign the return matrix
  mVaR = matrix(NA, nrow = objGarchFit@model$modeldata$T,
                 ncol = length(vQ))
  
  mVaR = switch(sDistrib, 
         norm = sapply(vQ, function(quantile) {
           vCondMean + vCondSD * qnorm(quantile)
         }),
         std = {
           dDoF = coef(objGarchFit)["shape"]
           sapply(vQ, function(quantile) {
             vCondMean + vCondSD * sqrt((dDoF - 2)/dDoF)*qt(quantile, df = dDoF)
           })
         }, 
         sstd = {
           dDoF = coef(objGarchFit)["shape"]
           dGamma = coef(objGarchFit)["skew"]
           sapply(vQ, function(quantile) {
             vCondMean + vCondSD * qskt(p = quantile,df = dDoF, gamma = dGamma) 
           })
         })
  colnames(mVaR) = paste0("VaR", vQ)
  rownames(mVaR) = as.character(objGarchFit@model$modeldata$index)
  return(mVaR)
}

# get the in-sample VaR
mVaR1 = getVaR(gfN1, c(0.05, 0.1, 0.9, 0.95))
mVaR2 = getVaR(gfN2, c(0.05, 0.1, 0.9, 0.95))
mVaR3 = getVaR(gfN3, c(0.05, 0.1, 0.9, 0.95))

# plot the in-sample VaR
dfVaR1 = cbind.data.frame(date = rownames(mVaR1), 
                 actual = gfN1@model$modeldata$data, 
                 fitted = fitted(gfN1), mVaR1)
dfVaR1L = melt(dfVaR1, id.var = "date")
ggplot(dfVaR1L, aes(x = as.Date(date), y = value, color = variable)) +
  geom_line() + theme_bw() + xlab("Date") + ylab("NASDAQ returns")