library(caret)
library(fscaret)

splitIndex <- createDataPartition(Iris$CLASS, p = .75, list = FALSE, times = 1)
trainDF <- totalbasefile[ splitIndex,]
testDF  <- totalbasefile[-splitIndex,]
myFS<-fscaret(trainDF, testDF, myTimeLimit = 10, preprocessData=TRUE,
              Used.funcRegPred = 'gbm', with.labels=TRUE,
              supress.output=FALSE, no.cores=2)