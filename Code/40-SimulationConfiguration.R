library(dplyr)

# read in the simultation configuration dataset
dfX = read.table(textConnection("
                 SOURCE  NSUB   MEAN   SD   DIST
Study1  10     1.5    0.3  0
Study2  5      2.5    0.4  1
Study1  4      3.5    0.3  0"),
                 header = TRUE, stringsAsFactors = FALSE)

# write a function that takes each row of the configuration
#   data.frame and returns the simulations
doSim = function(simConfig, seed = 12345) {
  set.seed(seed)
  dist = if(simConfig[["DIST"]] == 0) rnorm else rlnorm
  mean = if(simConfig[["DIST"]] == 0) simConfig[["MEAN"]] else log(simConfig[["MEAN"]]) 
  return(
    data_frame(
      source = simConfig[["SOURCE"]],
      nsub = simConfig[["NSUB"]],
      value = dist(1000, mean = mean, sd = simConfig[["SD"]])
    )
  )
}

# test the function
doSim(dfX[1, ])

# apply over dfX
dfX %>%
  rowwise() %>%
  do(doSim(.))