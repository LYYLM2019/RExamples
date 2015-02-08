library(reshape2)

# generate the combinations to iterate over
vInput = seq(1, 10)
dfSeq = expand.grid(rowSeq = seq(from = 0, by = 0.001, length.out = 3000),
            colSeq = seq(from = 0, by = 0.01, length.out = 20))

# generate the values
dfSeq = cbind.data.frame(result = mapply(function(row, col) {
  length(vInput)*sqrt((1-col)/col)*exp(row)
}, dfSeq$rowSeq, dfSeq$colSeq), dfSeq)

# cast them in the shape required
dfSeqWide = dcast(dfSeq, rowSeq~colSeq, value.var = "result")