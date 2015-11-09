library(reshape)

# example of data reshaping
dfSimplify = expand.grid(year = c(2012, 2013), 
                         week = 1:52, 
                         campus = 1:41, media = 1:16)
dfSimplify$data1 = rnorm(nrow(dfSimplify))
dfSimplify$data2 = rnorm(nrow(dfSimplify))

# melt the data
dfSimplifyMelt = reshape::melt(dfSimplify, id.vars = c('week', 'media', 'campus', 'year'))
dfSimplifyWide = reshape::cast(dfSimplifyMelt, year + week + campus ~ media + variable)