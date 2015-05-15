# genrate some sample data
vectorLengths = sample.int(10, size = 100, replace = TRUE)
listOfCharNum = lapply(
  lapply(
    vectorLengths, sample.int), as.character)

# get the length of the longest vector in the list
maxLen = max(sapply(listOfCharNum, length))
listOfCharNum = lapply(listOfCharNum,)

# extend the length of all the vectors
do.call(rbind, lapply(listOfCharNum, function(x) {
  if(length(x) !=10) x[(length(x) + 1):maxLen] <- NA
  as.numeric(x)
}))

# a cheaper way to do this would be to pre-assign the matrix, and 
#   