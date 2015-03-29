#====================================================================
# purpose: understand the performance of hash tables in R
# author: tirthankar chakravarty
# created: 27th march 2015
# comments:
# http://jeffreyhorner.tumblr.com/post/114524915928/hash-table-performance-in-r-part-i
#====================================================================

rm(list = ls())

library(plyr)
library(ggplot2)


getUniqueStrings = function(numStrings) {
  # drawing from a large namespace, no hash collisions expected
  replicate(numStrings, paste(sample(c(letters, LETTERS), 6), collapse = ""))
}

# vector
makeHash1 = function(lenHash) {
  vHash = integer(lenHash)
  names(vHash) = getUniqueStrings(lenHash)
  return(vHash)
}
makeHash1(10)
sapply(2^(10:15), function(x) {
 hash = makeHash(x)
 c(numeric = system.time(hash)[3])
})
