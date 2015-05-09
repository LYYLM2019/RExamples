# create a function that returns an object of class myClassifierClass
myClassifier = function(trainingData, ...) {
  model = structure(list(x = trainingData[, -1], y = trainingData[, 1]), 
                    class = "myClassifierClass") 
  return(model)
}

# create a method for function print for class myClassifierClass
predict.myClassifierClass = function(modelObject) {
  return(rlogis(length(modelObject$y)))
} 

# test
mA = matrix(rnorm(100*10), nrow = 100, ncol = 10)
modelA = myClassifier(mA)
predict(modelA)

