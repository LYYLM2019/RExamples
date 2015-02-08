# http://stats.stackexchange.com/q/134132/8141

# simulate some log-logistic data
library(FAdist)
vY = rllog(n = 1000, shape = 5, scale = 6)

# log-likelihood function
fnLLLL = function(vParams, vData) {
  # uses the density function of the log-logistic function from FAdist
  return(-sum(log(dllog(vData, shape = vParams[1], scale = vParams[2]))))
}

# optimize it
optim(c(2, 3), fnLLLL, vData = vY)
