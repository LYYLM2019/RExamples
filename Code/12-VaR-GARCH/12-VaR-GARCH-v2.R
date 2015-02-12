#==========================================================
# purpose: estimation of in-sample VaR using various GARCH
#   models
# These examples are taken from the G@RCH 6.1 manual (Section 
#   6.1: VaR models; http://www.timberlake.co.uk/slaurent/G@RCH/default.htm)
# The data is distributed with the console version of the G@RCH 
#   6.1 software
# author: TC
# created: 11th February 2015
# revised: 14th February 2015
# comments:
# TODO:
#==========================================================

library(xlsx)
library(xts)
library(rugarch)
library(skewt)
library(reshape2)
library(ggplot2)
library(fGarch) # for standardized Student-t distribution quantiles

# read in the data
dfN = read.xlsx("Data/nasdaq.xls", sheetIndex = 1)
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
# function to compute VaR for different distributions
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
               ## NOTE: The variance is finite for df > 2
               vCondMean + vCondSD * 
                   qstd(quantile, nu = dDoF) 
           })
         }, 
         sstd = {
           dDoF = coef(objGarchFit)["shape"]
           dGamma = coef(objGarchFit)["skew"]
           sapply(vQ, function(quantile) {
             ## NOTE: the variance is only finite for df > 4
               vCondMean + vCondSD *
                   qsstd(p = quantile, nu = dDoF, xi = dGamma)  
           })
       })
  colnames(mVaR) = paste0("VaR", vQ)
  rownames(mVaR) = as.character(objGarchFit@model$modeldata$index)
  return(mVaR)
}

#================================================
# compute the in-sample VaR
#================================================
# get the in-sample VaR
mVaR1 = getVaR(gfN1, c(0.1, 0.9))
mVaR2 = getVaR(gfN2, c(0.1, 0.9))
mVaR3 = getVaR(gfN3, c(0.1, 0.9))

# check the VaR computations
mean(gfN2@model$modeldata$data > mVaR2[, 2])
mean(gfN2@model$modeldata$data < mVaR2[, 1])

mean(gfN1@model$modeldata$data > mVaR1[, 2])
mean(gfN1@model$modeldata$data < mVaR1[, 1])

mean(gfN3@model$modeldata$data > mVaR3[, 2])
mean(gfN3@model$modeldata$data < mVaR3[, 1])

#================================================
# plot the in-sample VaR: skewed Student's t
#================================================
dfVaR2 = cbind.data.frame(date = rownames(mVaR2), 
                 actual = gfN2@model$modeldata$data, 
                 fitted = fitted(gfN2), mVaR2)
dfVaR2L = melt(dfVaR2, id.var = "date")
ggplot(dfVaR2L, aes(x = as.Date(date), y = value, color = variable)) +
  geom_line() + theme_bw() + xlab("Date") + ylab("NASDAQ returns")


#================================================
# compute the rolling forecasts
#================================================
grN1 = ugarchroll(gN1, data = xN, 
           n.start = min(which(index(xN) > as.Date("2000-01-01"))), 
           refit.every = 100, 
           calculate.VaR = TRUE, 
           VaR.alpha = c(0.1, 0.9))

# check the properties of this object
length(grN1@forecast)

# check how many have not converged
urN1Conv = sapply(grN1@forecast, function(x) x$converge)

# what does the forecast object contain
length(grN1@forecast[[1]]$Mu)


#================================================
# GARCH forecasts
#================================================
gfN1 = ugarchfit(gN1, data = xN, out.sample = 300)
temp = ugarchforecast(gfN1, n.ahead = 300, n.roll = 10)
