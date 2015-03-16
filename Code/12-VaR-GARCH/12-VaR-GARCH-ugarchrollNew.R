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
source("ugarchrollNew.R")

library(xlsx)
library(xts)
library(rugarch)
library(skewt)
library(reshape2)
library(ggplot2)

library(fGarch) # for standardized Student-t distribution quantiles

# read in the data
dfN = read.xlsx("nasdaq.xls", sheetIndex = 1)
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

# rolling forecast with n.ahead = 10
gforecastN1 = ugarchrollNew(spec = gN1, data = xN, n.ahead = 10,
                            n.ahead.extract = 10, forecast.length = 10)

# fit an ARMA(2, 0)-APARCH(1, 1) skewed-Student model
gN2 = ugarchspec(variance.model = list(model = "apARCH",
                                       garchOrder = c(1, 1)), 
                 mean.model = list(armaOrder = c(2, 0), 
                                   include.mean = TRUE),
                 distribution.model = "sstd")

gforecastN2 = ugarchrollNew(spec = gN2, data = xN, n.ahead = 10,
                            n.ahead.extract = 10, forecast.length = 10)

# fit an ARMA(2, 0)-APARCH(1, 1) Student-t model
gN3 = ugarchspec(variance.model = list(model = "apARCH",
                                       garchOrder = c(1, 1)), 
                 mean.model = list(armaOrder = c(2, 0), 
                                   include.mean = TRUE),
                 distribution.model = "std")

gforecastN3 = ugarchrollNew(spec = gN3, data = xN, n.ahead = 10,
                            n.ahead.extract = 10, forecast.length = 10)

#================================================
# compute the VaR
#================================================
getVaR = function(objGarchFor, vQ) {
  # get the distribution
  sDistrib = objGarchFor$model$distribution
  # get the conditional mean
  vCondMean = objGarchFor$forecast$Mu
  # get the conditional s.d.
  vCondSD = objGarchFor$forecast$Sigma
  
  # pre-assign the return matrix
  mVaR = matrix(NA, nrow = nrow(objGarchFor$forecast),
                 ncol = length(vQ))
  
  mVaR = switch(sDistrib, 
         norm = sapply(vQ, function(quantile) {
           vCondMean + vCondSD * qnorm(quantile)
         }),
         std = {
           dDoF = objGarchFor$forecast$Shape

           ## VaR
           sapply(vQ, function(quantile) {
               ## quantile of std
               qtlSTD = sapply(1:length(dDoF), function (i) {
                   qstd(p = quantile, nu = dDoF[i])
           })
               ## Using qstd instead of qt for computing quantile of a
               ## standardised Student-t distribution
               ## NOTE: The variance is finite for df > 2
               vCondMean + vCondSD * sqrt((dDoF - 2)/dDoF) *
                   qtlSTD
               ## vCondMean + vCondSD * sqrt((dDoF - 2)/dDoF)*
               ## qt(quantile, df = dDoF) 
           })
         }, 
         sstd = {
           dDoF = objGarchFor$forecast$Shape
           dGamma = objGarchFor$forecast$Skew
           
           ## VaR    
           sapply(vQ, function(quantile) {
               ## quantile of sstd
               qtlSSTD = sapply(1:length(dDoF), function (i) {
                   qsstd(p = quantile, nu = dDoF[i], xi = dGamma[i])
               })
               ## Variance of a GH skewed Student-t dist
               ## NOTE: the variance is only finite for df > 4
               varSSTD = (dGamma)^2/(dDoF -2) +
                   (2*((dGamma)^4)/((dDoF - 2)^2 * (dDoF - 4)))
               
               ## Using qsstd instead of qskt
               vCondMean + vCondSD * (1/sqrt(varSSTD)) *
                   qtlSSTD  
           })
       })
  colnames(mVaR) = paste0("VaR", vQ)

  ## TO DO: Changed rownames for forecasts lying outside the index
  forOnDate = as.Date(objGarchFor$forecast$ForecastOn)
  ## n.ahead.extract = objGarchFor$model$n.ahead.extract
  ## forVaRDate = objGarchFor$model$index[(which(
  ##     objGarchFor$model$index %in% forOnDate)) + n.ahead.extract]
  rownames(mVaR) = as.character(forOnDate)
  return(mVaR)
}

# get the in-sample VaR
mVaR1 = getVaR(gforecastN1, c(0.05, 0.1, 0.9, 0.95))
mVaR2 = getVaR(gforecastN2, c(0.05, 0.1, 0.9, 0.95))
mVaR3 = getVaR(gforecastN3, c(0.05, 0.1, 0.9, 0.95))

# plot the in-sample VaR
## dfVaR1 = cbind.data.frame(date = rownames(mVaR1), 
##                  actual = gfN1@model$modeldata$data, 
##                  fitted = fitted(gfN1), mVaR1)
## dfVaR1L = melt(dfVaR1, id.var = "date")
## ggplot(dfVaR1L, aes(x = as.Date(date), y = value, color = variable)) +
##   geom_line() + theme_bw() + xlab("Date") + ylab("NASDAQ returns")



