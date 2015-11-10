library(lazyeval)
library(dplyr)

# create the data.frame
dfX = data.frame(num1=seq(1,10,len=20),
                 num2=seq(20,30,len=20),
                 char1=c(rep('a',10), rep('b',10)),
                 target=c(rep(1,10), rep(0,10))
)

# select the numeric columns
numericCols = names(dfX)[sapply(dfX, is.numeric)]
numericCols = setdiff(numericCols, "target")

# cycle over numeric columns, creating summary data.frames
liDFY = setNames(
  lapply(
    numericCols, function(x) {
      # compute the quantiles
      quantiles = quantile(dfX[[x]], probs = seq(0, 1, 0.2))

      # create quantile membership
      dfX[["quantile_membership"]] =
        findInterval(dfX[[x]], vec = quantiles,
                     rightmost.closed = TRUE,
                     all.inside = TRUE)

      # summarize variables by decile
      dfX %>%
        group_by(quantile_membership)   %>%
        summarize_(min = interp( ~ min(x_name), x_name = as.name(x)),
                   max = interp( ~ max(x_name), x_name = as.name(x)),
                   mean = interp( ~ mean(x_name), x_name = as.name(x))) %>%
        mutate(varname = x)
    }),
  numericCols
)

# inspect the output
liDFY[[numericCols[1]]]
