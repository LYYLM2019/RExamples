library(xlsx)
library(xts)
library(rugarch)

######################################################################                ## Using and testing 'ugarchrollNew' ##
######################################################################
source("ugarchrollNew.R")

## data
dfN <- read.xlsx("nasdaq.xls", sheetIndex = 1)
xN <- xts(dfN$Nasdaq, order.by = dfN$Date)            

#######################################################################
                   ## Using ugarchrollNew ##
#######################################################################

## ARMA(2, 0)-APARCH(1, 1) Normal Model
gN1 <- ugarchspec(variance.model = list(model = "apARCH",
                      garchOrder = c(1, 1)), 
                  mean.model = list(armaOrder = c(2, 0), 
                      include.mean = TRUE),
                  distribution.model = "norm")

## Rolling forcast with model re-fit
forMod <- ugarchrollNew(spec = gN1, data = xN,
                        n.ahead = 10, n.ahead.extract = 10,
                        forecast.length = 15, refit.every = 1,
                        refit.window = "recursive")

#######################################################################
## NOTE:
## ForecastOn : time on which forecast is made. The fit is on till
##              t = ForecastOn.
## PeriodAhead: n.ahead.extract
## Mu         : Returns forecast
## Sigma      : conditional sd forecast
## Realized   : realized returns
## Rest are distribution parameter estimates
#######################################################################
head(forMod$forecast)

#######################################################################
         ## Testing the results from ugarchrollNew ##
#######################################################################

## model fit with out.sample = 15
gfN1 <- ugarchfit(gN1, data = xN, out.sample = 15)

## model fit with out.sample = 14 (Refitting by adding one more day)
gfN2 <- ugarchfit(gN1, data = xN, out.sample = 14)

## forecast
gforecastN1 <- ugarchforecast(fit = gfN1, n.ahead = 10, n.roll = 14)
gforecastN2 <- ugarchforecast(fit = gfN2, n.ahead = 10, n.roll = 14)

## Testing for sigma forecasts
test <- data.frame(forMod$forecast[1:2,],
                   forN1andN2 = c(gforecastN1@forecast$sigmaFor[10, 1],
                       gforecastN2@forecast$sigmaFor[10, 1]))

test$Mu == test$forN1andN2
## RESULT: Forecast of sigma from ugarchrollNew and from hnd-roll
##         matches, test$Mu = test$forN1andN1
