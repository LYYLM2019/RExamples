library(caret)
library(doParallel)
library(data.table)

cl <- makeCluster(detectCores() - 1) # I'm using 3 cores.
registerDoParallel(cl)

data(iris)
iris <- iris[iris$Species != 'virginica', ] # to get two categories
TrainData <- as.data.table(iris[,1:4]) # My data is a data.table.
TrainClasses = as.factor(as.character(iris[, 5]))
# TrainClasses <- factor(as.character(as.numeric(iris[,5]) - 1), 
#                        levels  = c("0", "1"),
#                        labels = c("setosa", "versicolor"),
#                        ordered = TRUE)  # to reset the levels to the two remaining flower types.
as.numeric(TrainClasses)

ctrl <- trainControl(method = 'oob',
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     allowParallel = TRUE)

debugonce(train)
model.fit <- train(x = TrainData,
                   y = TrainClasses,
                   method = 'rf',
                   metric = 'ROC',
                   tuneLength = 3,
                   trControl = ctrl)